package sudoku.model

/**
 * These items have an solved and an unsolved state
 * @author Mikko Hilpinen
 * @since 23.4.2020, v1
 */
trait Solvable
{
	// ABSTRACT	--------------------------
	
	/**
	 * @return Whether this item has already been solved
	 */
	def isSolved: Boolean
	
	
	// COMPUTED	--------------------------
	
	/**
	 * @return Whether this item hasn't been solved yet
	 */
	def nonSolved = !isSolved
}
