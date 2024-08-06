package sudoku.controller

import sudoku.model.{Grid, Slot, SolvableGroupType, SolveResult, SudokuState}
import sudoku.util.MultiMapBuilder
import utopia.paradigm.enumeration.Axis2D
import utopia.firmament.localization.LocalString
import utopia.paradigm.shape.shape2d.area.polygon.c4.bounds.Bounds
import utopia.paradigm.shape.shape2d.vector.point.Point
import utopia.paradigm.shape.shape2d.vector.size.Size

/**
 * This solver utilizes an interesting fact about a 45 slot sudoku board where the "center ring" (slots around (outside) the
 * center grid, including corners) must contain the same numbers as the four 2x2 corners. This part of the solver makes
 * sure the grid contains the corner numbers while the other half does the opposite.
 * @author Mikko Hilpinen
 * @since 26.4.2020, v1
 */
object CenterRingAndCornersMatcher
{
	// ATTRIBUTES	--------------------------
	
	private implicit val languageCode: String = "en"
	
	private val cornerBounds = Vector(0, 6).flatMap { startX =>
		Vector(0, 6).map { startY =>
			Bounds(Point(startX, startY), Size(2, 2))
		}
	}
	private val innerCenterBounds = Bounds(Point(3, 3), Size(3, 3))
	private val outerCenterBounds = Bounds(Point(2, 2), Size(5, 5))
	
	/**
	 * Different versions of this algorithm
	 */
	val variants = Vector(OutsideInSolver, InsideOutSolver)
	
	
	// OTHER	------------------------------
	
	private def solve(sudoku: SudokuState, name: => LocalString, isOutsideIn: Boolean) =
	{
		// Finds the areas that intersect with the source area (four corners or center ring)
		val grids = if (isOutsideIn) cornerGridsIn(sudoku) else centerGridsIn(sudoku)
		val lines = if (isOutsideIn) cornerLinesIn(sudoku) else centerLinesIn(sudoku)
		// Finds the source slots
		val sourceSlots = if (isOutsideIn) cornerSlotsIn(grids) else centerSlotsIn(grids)
		
		// Finds the numbers that already exist on the source (may contain duplicates)
		val placedNumbers = sourceSlots.flatMap { _.number }
		// Finds the numbers that are forced to exist on the source, even though exact location isn't known
		// (will not count duplicates for now, because there's overlap between the grids and the lines)
		val shadowNumbers = (grids ++ lines).flatMap { group =>
			val numbersToPlace = group.missingNumbers
			numbersToPlace.filter { number =>
				val availableSlots = group.emptySlots.filter { _.availableNumbers.contains(number) }
				// Only includes numbers that can only exist in the specified areas
				availableSlots.forall(sourceSlots.contains)
			}
		}.toSet
		
		if (placedNumbers.nonEmpty || shadowNumbers.nonEmpty)
		{
			// Next finds the target slots
			val targetSlots = if (isOutsideIn) centerRingIn(sudoku) else cornerSlotsIn(sudoku.grids)
			
			// Counts the required amount of each number (not exact because some shadow numbers may be skipped)
			val numbersWithCounts = (placedNumbers ++ shadowNumbers).groupMapReduce { i => i } { i => i } { _ + _ }
			// Counts the existing number of placed numbers within the target area
			val existingNumbersInTargetWithCounts = targetSlots.flatMap { _.number }.groupMapReduce {
				i => i } { i => i } { _ + _ }
			// Finds the numbers that still need to be placed there (places only one copy of each number at this time)
			val remainingNumbers = numbersWithCounts.map { case (number, count) => number -> (
				count - existingNumbersInTargetWithCounts.getOrElse(number, 0)) }.filter { _._2 > 0 }.keySet
			
			if (remainingNumbers.nonEmpty)
			{
				// Checks where those numbers can be placed within the target area
				val slotsPerNumber = remainingNumbers.map { number =>
					val availableSlots = targetSlots.filter { _.availableNumbers.contains(number) }
					number -> availableSlots
				}
				
				// If there's only a single place for the number, can place it straight away
				// Collection format: Slot -> number
				val onlyPlacePairs = slotsPerNumber.filter { _._2.size == 1 }.map { case (number, slots) =>
					slots.head -> number }.toMap
				
				// If there are multiple positions, checks whether those positions lay in the same x or y
				// -coordinate or in the same grid
				// group type -> [group index -> [Number -> [available slots]]]
				val sameGroupBuilders = SolvableGroupType.values.map { gType =>
					gType -> new MultiMapBuilder[Int, (Int, Vector[Slot])] }.toMap
				slotsPerNumber.filter { _._2.size > 1 }.foreach { case (number, slots) =>
					// Adds the slots to appropriate groups
					SolvableGroupType.values.foreach { gType =>
						val indices = slots.map { _.indexIn(gType) }.toSet
						if (indices.size == 1)
							sameGroupBuilders(gType) += indices.head -> (number -> slots)
					}
				}
				// Map(group) ->  map(index) -> (number -> only available slots in group)
				val sameGroupResults = sameGroupBuilders.view.mapValues { _.result() }.toMap
				
				if (onlyPlacePairs.nonEmpty || sameGroupResults.exists { _._2.nonEmpty })
				{
					// Performs the changes to the sudoku
					sudoku.trySolveSlots { slot =>
						// Checks for only number requirement
						onlyPlacePairs.get(slot) match
						{
							case Some(onlyPossibleNumber) => Some(slot.withMustBe(Set(onlyPossibleNumber)))
							case None =>
								// Checks for requirements based on slot position
								val newNotAllowed = sameGroupResults.flatMap { case (gType, results) =>
									results.getOrElse(slot.indexIn(gType), Vector())
										.filterNot { _._2.contains(slot) }.map { _._1 }
								}.toSet
								if (newNotAllowed.nonEmpty)
									Some(slot.withNotAllowed(newNotAllowed))
								else
									None
						}
					} match
					{
						case Some(result) =>
							val (newState, slots) = result
							val affectingSlots = (sourceSlots ++ targetSlots.filter { _.nonSolved }).toSet -- slots
							SolveResult.success(newState, slots.toSet, affectingSlots, Some(name))
						case None => SolveResult.failure(sudoku)
					}
				}
				else
					SolveResult.failure(sudoku)
			}
			else
				SolveResult.failure(sudoku)
		}
		else
			SolveResult.failure(sudoku)
	}
	
	private def cornerGridsIn(sudoku: SudokuState) = sudoku.grids.filter { _.slots.exists { slot => cornerBounds.exists {
		_.contains(slot.position.toPoint) } } }
	
	private def centerGridsIn(sudoku: SudokuState) = sudoku.grids.filterNot { grid =>
		innerCenterBounds.contains(grid.position.toPoint) }
	
	private def cornerLinesIn(sudoku: SudokuState) = Axis2D.values.flatMap { axis => sudoku.slotLinesAlong(axis).filter {
		_.slots.exists { slot => cornerBounds.exists { _.contains(slot.position.toPoint) } } } }
	
	private def centerLinesIn(sudoku: SudokuState) = Axis2D.values.flatMap { axis =>
		sudoku.slotLinesAlong(axis).filter { _.slots.exists { slot => outerCenterBounds.contains(slot.position.toPoint) } } }
	
	private def cornerSlotsIn(grids: Vector[Grid]) = grids.flatMap { _.slots.filter { slot => cornerBounds.exists {
		_.contains(slot.position.toPoint) } } }
	
	private def centerSlotsIn(grids: Vector[Grid]) = grids.flatMap { _.slots.filter { slot =>
		outerCenterBounds.contains(slot.position.toPoint) } }
	
	private def centerRingIn(sudoku: SudokuState) = sudoku.grids.flatMap { _.slots.filter { slot =>
		outerCenterBounds.contains(slot.position.toPoint) && !innerCenterBounds.contains(slot.position.toPoint) } }
	
	
	// NESTED	---------------------------
	
	private object OutsideInSolver extends SolveAlgorithm
	{
		override def name = "Corners to center ring -enforcing"
		
		override def apply(sudoku: SudokuState) = solve(sudoku, name, isOutsideIn = true)
	}
	
	private object InsideOutSolver extends SolveAlgorithm
	{
		override def name = "Center ring to four corners -enforcing"
		
		override def apply(sudoku: SudokuState) = solve(sudoku, name, isOutsideIn = false)
	}
}
