package sudoku.model

import utopia.firmament.localization.LocalString

object SolveResult
{
	/**
	 * A failure result. Returned when no advancement could be made in the sudoku
	 * @param sudoku Sudoku that was being solved
	 * @return A failure result
	 */
	def failure(sudoku: SudokuState) = SolveResult(wasSuccess = false, sudoku)
	
	/**
	 * A successful result, returned when some advancement could be made on the sudoku
	 * @param newState Sudoku after solving the next step
	 * @param modifiedSlots The slots that were modified
	 * @param relatedSlots Slots that affected the result
	 * @param description Description of step (optional)
	 * @return Success result
	 */
	def success(newState: SudokuState, modifiedSlots: Set[Slot], relatedSlots: Set[Slot] = Set(),
				description: Option[LocalString] = None) = SolveResult(wasSuccess = true, newState,
		modifiedSlots, relatedSlots, description)
}

/**
 * A representation of a solve attempt results
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
case class SolveResult(wasSuccess: Boolean, newState: SudokuState, modifiedSlots: Set[Slot] = Set(),
					   relatedSlots: Set[Slot] = Set(), description: Option[LocalString] = None)
{
	/**
	 * @return Whether this attempt didn't yield success
	 */
	def wasFailure = !wasSuccess
}
