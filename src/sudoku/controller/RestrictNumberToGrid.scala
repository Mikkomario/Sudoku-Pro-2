package sudoku.controller

import utopia.flow.util.CollectionExtensions._
import sudoku.model.{SolveResult, SudokuState}
import utopia.genesis.shape.Axis2D

/**
 * Test whether available grid number slots align themselves in a way that locks out the possibility of the
 * number existing in the same row or column
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
object RestrictNumberToGrid extends SolveAlgorithm
{
	private implicit val languageCode: String = "en"
	
	override def name = "Restricts numbers in a column/row to a single grid's area"
	
	override def apply(sudoku: SudokuState) =
	{
		// Goes through grids in order to find one that has effect on rows/columns
		// (restricted number + affected positions + slots that originated the change)
		val target = sudoku.grids.findMap { grid =>
			// Checks if some of the possible numbers in the grid are aligned
			grid.missingNumbers.findMap { number =>
				val possibleSlots = grid.emptySlots.filter { _.availableNumbers.contains(number) }
				Axis2D.values.findMap { axis =>
					val coordinates = possibleSlots.map { _.position.along(axis) }.toSet
					if (coordinates.size == 1)
					{
						// Checks whether this alignment would affect other slots in the line
						val targetLineIndex = coordinates.head
						val line = sudoku.slotLine(targetLineIndex, axis.perpendicular)
						val affectedSlots = line.emptySlots.filterNot { slot =>
							grid.containsPosition(slot.position) }.filter { _.availableNumbers.contains(number) }
						// Remembers the affected slots and the line index
						if (affectedSlots.nonEmpty)
						{
							println(s"Slots [${possibleSlots.map { _.position }.mkString(", ")}] are the only places in grid for $number")
							println(s"Therefore restricts this number from [${affectedSlots.map { _.position }.mkString(", ")}]")
							println("Available numbers in those slots before alteration:")
							affectedSlots.foreach { slot =>
								println(s"${slot.position}: [${slot.availableNumbers.toVector.sorted.mkString(", ")}]")
							}
							Some((number, affectedSlots.map { _.position }, possibleSlots))
						}
						else
							None
					}
					else
						None
				}
			}
		}
		
		// Modifies the target line, if one was found
		target match
		{
			case Some(foundTargets) =>
				val (number, positions, originators) = foundTargets
				// Modifies the grids that contain the slots which were selected earlier
				sudoku.trySolveGrids { grid =>
					if (positions.exists(grid.containsPosition))
					{
						grid.trySolveFlatMap { slot =>
							if (positions.contains(slot.position))
								Some(slot.withNotAllowed(Set(number)))
							else
								None
						}
					}
					else
						None
				} match
				{
					// Returns the original set of aligned slots as affecting slots
					case Some(result) => SolveResult.success(result._1, result._2.flatMap { _._2 }.toSet,
						originators.toSet, Some(name))
					case None => SolveResult.failure(sudoku)
				}
			case None => SolveResult.failure(sudoku)
		}
	}
}
