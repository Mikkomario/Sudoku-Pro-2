package vf.sudoku.controller.solve

import vf.sudoku.model.grid.{FullSlotsGroup, Slot}
import utopia.paradigm.enumeration.Axis2D
import vf.sudoku.model.solve.SudokuState

/**
 * A common trait for algorithms that target all slot group types independently and stop the modifications after
 * a single group has been altered
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
trait SolveNextGroup extends GroupSolve[FullSlotsGroup[_], (FullSlotsGroup[_], Vector[Slot])]
{
	// IMPLEMENTED	----------------------
	
	override protected def solveGridsIn(sudoku: SudokuState) =
		sudoku.trySolveNextGrid(solve)
	
	override protected def solveLinesIn(sudoku: SudokuState, axis: Axis2D) =
		sudoku.trySolveNextLine(axis)(solve)
	
	override protected def parseResult(result: (FullSlotsGroup[_], Vector[Slot])) =
		result._1 -> result._2.toSet
}
