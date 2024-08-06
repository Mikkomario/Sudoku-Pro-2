package vf.sudoku.view.reflection

import vf.sudoku.model.grid.{Coordinate, Grid, Slot}
import utopia.firmament.model.enumeration.WindowResizePolicy.Program
import utopia.flow.collection.CollectionExtensions._
import utopia.paradigm.generic.ParadigmDataType
import utopia.reflection.container.swing.window.Frame
import utopia.reflection.util.SingleFrameSetup
import vf.sudoku.controller.solve.Solver
import vf.sudoku.model.solve.SudokuState
import vf.sudoku.view.reflection.vc.MainVC

/**
 * The main app class for this project
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
object SudokuProApp extends App
{
	ParadigmDataType.setup()
	
	import DefaultContext._
	
	// Sets up localization context
	implicit val defaultLanguageCode: String = "EN"
	
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
	/*
	This very hard sudoku - requires advanced techniques
	"x2xxxxxx6", "18x3x6xx4", "x3xxxxxxx",
		"xxx1x5x94", "5xxxxxxx2", "41x2x8xxx",
		"xxxxxxx6x", "2xx8x5x31", "7xxxxxx2x"
	 */
	/* The world's hardest sudoku - wasn't able to deduct a single number
	"8xxxx3x7x", "xxx6xxx9x", "xxxxxx2xx",
		"x5xxxxxxx", "xx7x451xx", "xxx7xxx3x",
		"xx1xx8x9x", "xxx5xxxxx", "x68x1x4xx"
	 */
	/*
	Evil sudoku, required 1 manual deduction (naked twins + chained half-numbers)
	"xx2x9x4xx", "xx3x6x9xx", "xx4x8x6xx",
		"1xxx5xxx3", "2xxx4xxx5", "7xxx6xxx1",
		"xx9x3x7xx", "xx7x8x1xx", "xx2x7x8xx"
	 */
	/*
	Very difficult but not very interesting sudoku. Required 2 x-wings and 1 manual chain deduction
	"xx4x8x3xx", "xx5x4x7xx", "xx8x5x6xx",
		"6xxx3xxx9", "4xxx2xxx1", "3xxx4xxx7",
		"xx5x6x2xx", "xx2x8x9xx", "xx1x7x4xx"
	 */
	
	// These seem to be quite difficult: https://www.extremesudoku.info/
	// Could use a swordfish
	val sudoku = SudokuState(Vector(
		"xx2x9x4xx", "xx3x6x9xx", "xx4x8x6xx",
		"1xxx5xxx3", "2xxx4xxx5", "7xxx6xxx1",
		"xx9x3x7xx", "xx7x8x1xx", "xx2x7x8xx").mapWithIndex { (str, index) => stringToGrid(str, index)
	}.toVector)
	
	// Creates the component to display
	val content = new MainVC(sudoku)
	// new SudokuVC(sudoku).framed(16.any.square, Color.white)
	
	// Starts test
	private val frame = Frame.windowed(content, "SudokuPro", Program)
	frame.setToCloseOnEsc()
	new SingleFrameSetup(actorHandler, frame).start()
	
	Solver.algorithms.foreachWithIndex { (alg, idx) => println(s"$idx: ${alg.name}") }
}
