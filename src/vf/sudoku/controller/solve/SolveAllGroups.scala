package vf.sudoku.controller.solve

import vf.sudoku.model.grid.{FullSlotsGroup, Slot}
import utopia.paradigm.enumeration.Axis2D
import vf.sudoku.model.grid.{FullSlotsGroup, Slot}
import vf.sudoku.model.solve.SudokuState

/**
 * A common trait for algorithms that target all slot group types independently and try to modify each one of
 * the groups of that type
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
trait SolveAllGroups extends GroupSolve[Vector[FullSlotsGroup[_]], Vector[(FullSlotsGroup[_], Vector[Slot])]]
{
	// IMPLEMENTED	----------------------
	
	override protected def solveGridsIn(sudoku: SudokuState) =
		sudoku.trySolveGrids(solve)
	
	override protected def solveLinesIn(sudoku: SudokuState, axis: Axis2D) =
		sudoku.trySolveLinesAlong(axis)(solve)
	
	override protected def parseResult(result: Vector[(FullSlotsGroup[_], Vector[Slot])]) =
		result.map { _._1 } -> result.flatMap { _._2 }.toSet
}
