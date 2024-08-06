package vf.sudoku.controller.solve

import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Empty
import utopia.paradigm.enumeration.Axis.{X, Y}
import utopia.paradigm.enumeration.Axis2D
import vf.sudoku.model.grid.Coordinate
import vf.sudoku.model.solve.{SolveResult, SudokuState}
import vf.sudoku.util.MultiMapBuilder

object XWing
{
	/**
	 * Variants of this algorithm that each target different axes
	 */
	val variants = Axis2D.values.map { new XWing(_) }
}

/**
 * Finds an X-wing from the sudoku, based on half-number pairs
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
class XWing(axis: Axis2D) extends SolveAlgorithm
{
	private implicit val languageCode: String = "en"
	
	override def name = "Finds x-wings"
	
	override def apply(sudoku: SudokuState) =
	{
		val lines = sudoku.slotLinesAlong(axis)
		// Collects half-number pairs along the lines
		// number + aligned (slot) index 1 + aligned (slot) index 2 => [Perpendicular (line) index]
		val pairsBuilder = new MultiMapBuilder[(Int, Int, Int), Int]
		lines.foreachWithIndex { (line, perpendicularIndex) =>
			// Number -> Number's indices in this line
			val alignedIndicesPerNumber = line.slots.zipWithIndex
				.flatMap { case (slot, alignedIndex) =>
					slot.halfPlacesFor(axis).map { number => number -> alignedIndex }
				}
				.groupMap { _._1 } { _._2 }
			
			// Adds the found pairs to the map
			alignedIndicesPerNumber.foreach { case (number, alignedIndices) =>
				if (alignedIndices.size > 1)
				{
					val key = (number, alignedIndices.head, alignedIndices(1))
					pairsBuilder += (key -> perpendicularIndex)
				}
			}
		}
		val pairs = pairsBuilder.result()
		
		// Checks if there were any two lines where same number occupied the same slots
		// If there were, finds out all the slots that share a row or a column with the corner slots lines and makes sure
		// they won't contain that number
		val matchingPairs = pairs.filter { _._2.size > 1 }
		
		if (matchingPairs.nonEmpty)
		{
			// Collects each number that is now invalidated on each affected row and column index
			// Target Coordinate -> Limited Number
			val limitedPerpendicularIndicesBuilder = new MultiMapBuilder[Int, Int]()
			val limitedAlignedIndicesBuilder = new MultiMapBuilder[Int, Int]()
			// These registered x-wing corners can still hold the numbers
			// Position -> Numbers
			val protectedNumbersBuilder = new MultiMapBuilder[Coordinate, Int]
			matchingPairs.foreach { case (key, perpendicularIndices) =>
				val (number, alignIndex1, alignIndex2) = key
				val alignedIndices = Vector(alignIndex1, alignIndex2)
				// Registers limited numbers
				limitedPerpendicularIndicesBuilder ++= (perpendicularIndices, number)
				limitedAlignedIndicesBuilder ++= (alignedIndices, number)
				// Registers corners
				perpendicularIndices.foreach { perpendicularIndex =>
					alignedIndices.foreach { alignedIndex =>
						val position = axis match {
							case X => Coordinate(alignedIndex, perpendicularIndex)
							case Y => Coordinate(perpendicularIndex, alignedIndex)
						}
						protectedNumbersBuilder += position -> number
					}
				}
			}
			// Coordinate -> Restricted numbers
			val (limitedByX, limitedByY) = axis match {
				case X => limitedAlignedIndicesBuilder.result() -> limitedPerpendicularIndicesBuilder.result()
				case Y => limitedPerpendicularIndicesBuilder.result() -> limitedAlignedIndicesBuilder.result()
			}
			val protectedNumbers = protectedNumbersBuilder.result()
			
			sudoku.trySolveSlots { slot =>
				// Checks which number limits apply to this slot position
				val restrictedNumbers = (limitedByX.getOrElse(slot.position.x, Vector()) ++
					limitedByY.getOrElse(slot.position.y, Vector())).toSet
				if (restrictedNumbers.nonEmpty) {
					val cantBeNumbers = restrictedNumbers -- protectedNumbers.getOrElse(slot.position, Empty)
					if (cantBeNumbers.nonEmpty) {
						val newSlot = slot.withNotAllowed(cantBeNumbers)
						if (newSlot == slot)
							None
						else
							Some(newSlot)
					}
					else
						None
				}
				else
					None
			} match
			{
				case Some(result) =>
					val (newState, modifiedSlots) = result
					// Finds the x-wing corners as related slots
					val affectingSlots = protectedNumbers.keySet.map { position => newState.slotAt(position) }
					SolveResult.success(newState, modifiedSlots.toSet, affectingSlots, Some(name))
				
				case None => SolveResult.failure(sudoku)
			}
		}
		else
			SolveResult.failure(sudoku)
	}
}
