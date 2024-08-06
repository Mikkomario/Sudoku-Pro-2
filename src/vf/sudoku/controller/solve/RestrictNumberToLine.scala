package vf.sudoku.controller.solve

import utopia.paradigm.enumeration.Axis2D
import vf.sudoku.model.solve.{SolveResult, SudokuState}

/**
 * Checks whether there are half-number pairs (single pairs) inside a single line in a grid.
 * That means that the number can't appear anywhere else in the grid.
 * (Eg. if number 6 must be in the last column in first grid row,
 * it can't be on the other 2 columns in that grid row)
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
object RestrictNumberToLine extends SolveAlgorithm
{
	private implicit val languageCode: String = "en"
	
	override def name = "Restrict column and row half-places in grid"
	
	override def apply(sudoku: SudokuState) =
	{
		sudoku.trySolveNextGrid { grid =>
			// Finds the two required positions for each restricted number
			// Number -> [Only allowed positions]
			val restrictions = Axis2D.values
				.flatMap { axis =>
					val lines = grid.lines(axis)
					lines.flatMap { line =>
						val allHalfNumbersWithPositions = line.flatMap { slot => slot.halfPlacesFor(axis).map { _ -> slot.position } }
						val uniqueHalfNumbers = allHalfNumbersWithPositions.map { _._1 }.toSet
						val doubleHalfNumbers = uniqueHalfNumbers.filter { number => allHalfNumbersWithPositions.count { _._1 == number } > 1 }
						
						if (doubleHalfNumbers.nonEmpty)
						{
							// println(s"Found double half-numbers in grid ${grid.position}: [${doubleHalfNumbers.mkString(", ")}]")
							allHalfNumbersWithPositions.filter { case (number, _) => doubleHalfNumbers.contains(number) }
						}
						else
							Vector()
					}
				}
				.groupMap { _._1 } { _._2 }
			
			if (restrictions.nonEmpty)
			{
				// Next applies the limitations. Found half-numbers cannot exist anywhere else than on the positions they were
				// Found from
				grid.trySolveFlatMap { slot =>
					val restrictedNumbers = restrictions.filterNot { _._2.contains(slot.position) }.keySet
					if (restrictedNumbers.nonEmpty)
					{
						//println(s"Restricting slot ${slot.position} to [${restrictedNumbers.mkString(", ")}]")
						Some(slot.withNotAllowed(restrictedNumbers))
					}
					else
						None
				}
			}
			else
				None
		} match
		{
			case Some(result) =>
				// Can't generate affecting slots at this time
				SolveResult.success(result._1, result._2._2.toSet, description = Some(name))
			case None => SolveResult.failure(sudoku)
		}
	}
}
