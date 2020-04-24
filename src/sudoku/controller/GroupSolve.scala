package sudoku.controller

import sudoku.model.SolvableGroupType.{Column, Grid, Row}
import sudoku.model._
import utopia.genesis.shape.Axis.{X, Y}
import utopia.genesis.shape.Axis2D

/**
 * A common trait for algorithms that target all slot group types independently
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
trait GroupSolve[GG, Change] extends SolveAlgorithm
{
	// ABSTRACT	--------------------------
	
	/**
	 * @return The slot group type targeted by this instance
	 */
	def targetType: SolvableGroupType
	
	/**
	 * Solves a single group of items
	 * @param group Targeted group of items
	 * @tparam G Type of group being solved
	 * @return Modified group, along with a list of modified slots in that group. None if the group was not modified.
	 */
	protected def solve[G <: FullSlotsGroup[G]](group: G): Option[(G, Vector[Slot])]
	
	/**
	 * Finds the slots that affected the solving of the modified slots
	 * @param from group data from which slots can be retrieved
	 * @param modifiedSlots Slots that were modified
	 * @return Slots that affected the solving of the modified slots
	 */
	protected def affectingSlots(from: GG, modifiedSlots: Set[Slot]): Set[Slot]
	
	/**
	 * Performs the 'solve' function on sudoku grids
	 * @param sudoku Targeted sudoku
	 * @return New sudoku state + registered changes. None if sudoku was not changed.
	 */
	protected def solveGridsIn(sudoku: SudokuState): Option[(SudokuState, Change)]
	
	/**
	 * Performs the 'solve' function on sudoku lines
	 * @param sudoku Targeted sudoku
	 * @param axis Axis of the targeted lines
	 * @return New sudoku state + registered changes. None if sudoku was not changed.
	 */
	protected def solveLinesIn(sudoku: SudokuState, axis: Axis2D): Option[(SudokuState, Change)]
	
	/**
	 * Parses modified slots and group data from changes
	 * @param result Changes returned by sudoku solve
	 * @return Data concerning modified group(s) + modified slots as a set
	 */
	protected def parseResult(result: Change): (GG, Set[Slot])
	
	
	// IMPLEMENTED	----------------------
	
	override def apply(sudoku: SudokuState) =
	{
		// Performs the mapping based on target group type
		(targetType match
		{
			case Grid => solveGridsIn(sudoku)
			case Row => solveLinesIn(sudoku, X)
			case Column => solveLinesIn(sudoku, Y)
		}) match
		{
			case Some(result) =>
				// Collects modified slots + affected group(s)
				val (groupData, modifiedSlots) = parseResult(result._2)
				SolveResult.success(result._1, modifiedSlots, affectingSlots(groupData, modifiedSlots), Some(name))
			case None => SolveResult.failure(sudoku)
		}
	}
}
