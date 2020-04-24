package sudoku.controller

import sudoku.model.SolvableGroupType.{Column, Row}
import sudoku.model.SudokuState
import utopia.flow.util.CollectionExtensions._
import utopia.genesis.shape.Axis2D

/**
 * Checks whether there are half-number pairs (single pairs) inside a single line in a grid.
 * That number can't appear anywhere else in the grid. (Eg. if number 6 must be in the last column in first grid row,
 * it can't be on the other 2 columns in that grid row)
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
object RestrictNumberToLine // extends SolveAlgorithm
{
	/*
	private implicit val languageCode: String = "en"
	
	override def name = "Restrict column and row half-places in grid"
	
	override def apply(sudoku: SudokuState) =
	{
		sudoku.trySolveNextGrid { grid =>
			// Finds half-number pairs for columns and rows within this grid
			// Axis -> [Indices on axis -> [half numbers]]
			val restrictions = Axis2D.values.flatMap { axis =>
				val lines = grid.lines(axis)
				val restrictedNumbers = lines.mapWithIndex { (line, index) =>
					val allHalfNumbers = line.flatMap { _.halfPlacesFor(axis) }
					val uniqueHalfNumbers = allHalfNumbers.toSet
					val doubleHalfNumbers = uniqueHalfNumbers.filter { number => allHalfNumbers.count { _ == number } > 1 }
					
					if (doubleHalfNumbers.nonEmpty)
						Some(index -> doubleHalfNumbers)
					else
						None
				}.flatten
				
				if (restrictedNumbers.nonEmpty)
					Some(axis -> restrictedNumbers)
				else
					None
			}
			
			if (restrictions.nonEmpty)
			{
				val restrictionsForSlots =
			}
			???
		}
		???
	}*/
}
