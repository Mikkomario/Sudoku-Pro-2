package vf.sudoku.controller.solve

import vf.sudoku.model.solve.{SolveResult, SudokuState}

/**
 * This algorithm simply puts in the next number that is the only possible solution in a slot
 * @author Mikko Hilpinen
 * @since 23.4.2020, v1
 */
object SetNextOnlyPossibleNumber extends SolveAlgorithm
{
	private implicit val languageCode: String = "en"
	
	override def name = "Fills in the only possible number"
	
	override def apply(sudoku: SudokuState) =
	{
		sudoku.trySolveMapNextSlot { slot =>
			val numbers = slot.availableNumbers
			if (numbers.size == 1)
				slot.copy(number = Some(numbers.head))
			else
				slot
		} match
		{
			case Some(result) => SolveResult.success(result._1, Set(result._2._2), description = Some(name))
			case None => SolveResult.failure(sudoku)
		}
	}
}
