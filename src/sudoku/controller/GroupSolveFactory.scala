package sudoku.controller

import sudoku.model.SolvableGroupType

/**
 * A common trait for factory objects that produces solvers targeting all groups individually
 * @author Mikko Hilpinen
 * @since 25.4.2020, v1
 */
trait GroupSolveFactory[+A]
{
	// ABSTRACT	-------------------------
	
	/**
	 * @param targetGroupType Targeted group type
	 * @return A solver for that group type
	 */
	def apply(targetGroupType: SolvableGroupType): A
	
	
	// COMPUTED	-------------------------
	
	/**
	 * @return All variations of this solver
	 */
	def variations = SolvableGroupType.values.map(apply)
}
