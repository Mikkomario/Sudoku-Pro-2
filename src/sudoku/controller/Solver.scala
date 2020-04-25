package sudoku.controller

import utopia.flow.util.CollectionExtensions._

import sudoku.model.{SolveResult, SudokuState}

/**
 * An algorithm for solving a sudoku step by step
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
object Solver
{
	// ATTRIBUTES	--------------------------
	
	/**
	 * The algorithms used by this solver
	 */
	val algorithms = Vector(SetNextOnlyPossibleNumber) ++
		(RuleOutNonHalfPairs.variants :+ RestrictNumberToLine) ++
		TrimNumbers.variants ++ (FindOnlyPlaceForNumber.variants :+ RestrictNumberToGrid) ++
		NakedTwinsRule.variations ++ FindHalfPlaces.variants ++ XWing.variants :+ ClosedChainsRule
	
	/**
	 * Attempts to solve the next step in the sudoku (the next step may be information only)
	 * @param sudoku Sudoku to solve
	 * @return Result of solve attempt
	 */
	def apply(sudoku: SudokuState) =
	{
		// Attempts the algorithms in succession from simple to more advanced until a success is found or runs out
		// of algorithms
		algorithms.findMap { algorithm =>
			val result = algorithm(sudoku)
			if (result.wasSuccess)
				Some(result)
			else
				None
		}.getOrElse { SolveResult.failure(sudoku) }
	}
}
