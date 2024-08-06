package vf.sudoku.controller.solve

import utopia.firmament.localization.LocalString
import vf.sudoku.model.solve.{SolveResult, SudokuState}

/**
 * An enumeration for different sudoku solve steps / methods
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
trait SolveAlgorithm
{
	/**
	 * @return Name of this algorithm
	 */
	def name: LocalString
	
	/**
	 * Tries to solve the next step of the sudoku using this algorithm
	 * @param sudoku Sudoku to solve
	 * @return Result of solving
	 */
	def apply(sudoku: SudokuState): SolveResult
}