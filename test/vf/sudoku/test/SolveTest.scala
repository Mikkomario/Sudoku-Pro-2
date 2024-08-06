package vf.sudoku.test

import utopia.flow.async.process.Wait
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.time.TimeExtensions._
import vf.sudoku.controller.solve.Solver
import vf.sudoku.model.grid.{Coordinate, Grid, Slot}
import vf.sudoku.model.solve.{SolveResult, SudokuState}
import vf.sudoku.view.reflection.DefaultContext._

/**
 * Attempts to solve a sudoku (non-visual)
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
object SolveTest extends App
{
	// Creates a test sudoku
	def stringToGrid(str: String, gridIndex: Int) =
	{
		val gridX = gridIndex % 3 * 3
		val gridY = gridIndex / 3 * 3
		val slots = str.map { c => if (c.isDigit) Some(c.asDigit) else None }.mapWithIndex { (num, i) =>
			val x = i % 3 + gridX
			val y = i / 3 + gridY
			Slot(Coordinate(x, y), gridIndex, num)
		}.toVector
		Grid(slots)
	}
	val sudoku = SudokuState(Vector(
		"xx2xx4x9x", "xxxx1xx54", "3xxxx6xx8",
		"xxx93x4x7", "x2xxxxx6x", "7x4x52xxx",
		"6xx3xxxx9", "17xx9xxxx", "x3x2xx5xx").mapWithIndex { (str, index) => stringToGrid(str, index)
	}.toVector)
	
	println(s"Start:\n${sudoku.ascii}")
	
	val slotView = sudoku.slotsView
	
	val testGrid = sudoku.items.head
	assert(Grid.fromRows(testGrid.rows.toVector) == testGrid)
	assert(Grid.fromColumns(testGrid.columns) == testGrid)
	
	/*
	println("Rows:")
	println(slotView.rows.map { _.map { _.ascii }.mkString("|") }.mkString("\n"))
	
	println("Grid columns:")
	println(sudoku.columns.map { _.map { _.asciiRows.mkString("\n") }.mkString("\n\n") }.mkString("\n-------\n"))
	
	println("Columns:")
	println(slotView.columns.map { _.map { _.ascii }.mkString("|") }.mkString("\n"))
	*/
	
	// Attempts to solve the sudoku
	val waitLock = new AnyRef
	var lastResult = SolveResult.failure(sudoku)
	var stepIndex = 0
	do
	{
		stepIndex += 1
		lastResult = Solver(lastResult.newState)
		if (lastResult.wasSuccess)
		{
			println(s"$stepIndex Success; ${lastResult.description.map { _.string }.getOrElse("") }; ${
				lastResult.modifiedSlots.toVector.sortBy { _.availableNumbers.size }.mkString(", ") }.")
			// println(lastResult.newState.ascii)
		}
		else
			println("Failure")
		
		Wait(0.2.seconds, waitLock)
	}
	while (lastResult.wasSuccess && !lastResult.newState.isSolved)
	
	println(s"End:\n${lastResult.newState.ascii}")
}
