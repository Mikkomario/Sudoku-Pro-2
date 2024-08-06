package vf.sudoku.model.solve

import vf.sudoku.model.grid.{Coordinate, FlatGridView, Grid, GridFactory, GridLike, Slot, SlotLine}
import utopia.flow.collection.CollectionExtensions._
import utopia.paradigm.enumeration.Axis.{X, Y}
import utopia.paradigm.enumeration.Axis2D
import utopia.paradigm.shape.template.HasDimensions

object SudokuState extends GridFactory[Grid, SudokuState]

/**
 * Represents a single state of a sudoku puzzle
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
case class SudokuState(grids: Vector[Grid]) extends GridLike[Grid, SudokuState]
{
	// ATTRIBUTES	-------------------
	
	override val sideLength = 3
	
	/**
	 * A graph representation of the half-pair connections in this sudoku
	 */
	lazy val halfPairsGraph = HalfPairsGraph(grids)
	
	/**
	 * A graph representation of all same group connections between unsolved nodes
	 */
	lazy val groupAssociationsGraph = NumberAssociationGraph(grids)
	
	
	// COMPUTED	-----------------------
	
	/**
	 * @return A view directly to this sudoku's slots
	 */
	lazy val slotsView = new FlatGridView[Slot, Grid, SudokuState](this)({ slots => Grid(slots) })
	
	/**
	 * @return Rows of slots within this sudoku (left to right)
	 */
	def slotRows = slotsView.rows.map { row => SlotLine(row) }
	
	/**
	 * @return Columns of slots in this sudoku (top to bottom)
	 */
	def slotColumns = slotsView.columns.map { col => SlotLine(col) }
	
	/**
	 * @return An ascii representation of this sudoku state
	 */
	def ascii =
	{
		val gridRows = rows.map { gridRow =>
			val rowsPerGrid = gridRow.map { _.asciiRows }
			val numberOfRows = rowsPerGrid.map { _.size }.min
			val rowStrings = (0 until numberOfRows).map { rowIndex =>
				s"||\t${rowsPerGrid.map { _(rowIndex) }.mkString("\t||\t")}\t||"
			}
			rowStrings.mkString("\n")
		}
		val rowLength = gridRows.head.length
		val topBottom = s"++${Iterator.fill(rowLength - 4)("=").mkString("")}++"
		val rowSeparator = s"||${Iterator.fill(rowLength - 4)("=").mkString("")}||"
		s"$topBottom\n${gridRows.mkString(s"\n$rowSeparator\n")}\n$topBottom"
	}
	
	
	// IMPLEMENTED	-------------------
	
	override def repr = this
	
	override def withItems(newItems: Vector[Grid]) = copy(grids = newItems)
	
	override def items = grids
	
	
	// OTHER	-----------------------
	
	/**
	 * @param lineIndex Index of the line (x-coordinate for rows (x) and y-coordinate for columns (y))
	 * @param axis Target axis
	 * @return Slot axis along specified axis at specified index
	 */
	def slotLine(lineIndex: Int, axis: Axis2D): SlotLine = slotsView.lines(axis)(lineIndex).toVector
	
	/**
	 * @param position Target position
	 * @return Slot at specified position
	 */
	def slotAt(position: HasDimensions[Int]) = slotsView.rows(position.y)(position.x)
	
	/**
	 * @param axis Targeted axis
	 * @return Slot lines along the specified axis
	 */
	def slotLinesAlong(axis: Axis2D) = slotsView.lines(axis).map { slots => SlotLine(slots.toVector) }
	
	/**
	 * Adds a number to this sudoku
	 * @param slotPosition Position where the number is set
	 * @param number Number to be set
	 * @return A new version of this sudoku
	 */
	def withSlotNumber(slotPosition: Coordinate, number: Option[Int]) = copy(grids = grids.mapFirstWhere {
		_.containsPosition(slotPosition) } { grid => grid.withItems(grid.slots.mapFirstWhere {
		_.position == slotPosition } { _.copy(number = number) }) })
	
	/**
	 * Adds a number to this sudoku
	 * @param slotPosition Position where the number is set
	 * @param number Number to be set
	 * @return A new version of this sudoku
	 */
	def withSlotNumberNotAllowed(slotPosition: Coordinate, number: Int) = copy(grids = grids.mapFirstWhere {
		_.containsPosition(slotPosition) } { grid => grid.withItems(grid.slots.mapFirstWhere {
		_.position == slotPosition } { _.withNotAllowed(Set(number)) }) })
	
	/**
	 * Tries the solving function on slots within this sudoku until a change can be made or all slots have been tested
	 * @param f A slot solving function
	 * @return New state of this sudoku + modified slot. None if there was no change.
	 */
	def trySolveMapNextSlot(f: Slot => Slot) = trySolveNext { grid => grid.trySolveMapNext(f) }
	
	/**
	 * Tries the solving function on grids within this sudoku until a change can be made or all grids have been tested
	 * @param f A grid solving function (returns none if there was no change)
	 * @return A modified copy of this sudoku + modified grid + list of modified slots
	 */
	def trySolveNextGrid(f: Grid => Option[(Grid, Vector[Slot])]) = trySolveNext(f)
	
	/**
	 * Tries the solving function on lines in this sudoku until a change can be made or all lines have been tested
	 * @param axis Line axis
	 * @param f Line solving function. Returns modified line + changed slots. Returns None if no change was made.
	 * @return New state of this sudoku + modified line + modified slots. None if no change was made
	 */
	def trySolveNextLine(axis: Axis2D)(f: SlotLine => Option[(SlotLine, Vector[Slot])]) =
	{
		trySolveNext(slotLinesAlong(axis).toVector)(f){ newLines => withSlotLines(axis, newLines) }
	}
	
	/**
	 * Tries the solving function on rows in this sudoku until a change can be made or all rows have been tested
	 * @param f Line solving function. Returns modified row + changed slots. Returns None if no change was made.
	 * @return New state of this sudoku + modified row + modified slots. None if no change was made
	 */
	def trySolveNextRow(f: SlotLine => Option[(SlotLine, Vector[Slot])]) = trySolveNextLine(X)(f)
	
	/**
	 * Tries the solving function on columns in this sudoku until a change can be made or all columns have been tested
	 * @param f Line solving function. Returns modified column + changed slots. Returns None if no change was made.
	 * @return New state of this sudoku + modified column + modified slots. None if no change was made
	 */
	def trySolveNextColumn(f: SlotLine => Option[(SlotLine, Vector[Slot])]) = trySolveNextLine(Y)(f)
	
	/**
	 * Applies specified solving function to all slots in this sudoku
	 * @param f A function applied to the slots
	 * @return Modified version of this sudoku + list of all modified slots
	 */
	def trySolveMapSlots(f: Slot => Slot) = trySolve { grid => grid.trySolveMap(f) }
		.map { case (newState, changes) => newState -> changes.flatten }
	
	/**
	 * Applies specified solving function to all slots in this sudoku
	 * @param f A function applied to the slots
	 * @return Modified version of this sudoku + list of all modified slots
	 */
	def trySolveSlots(f: Slot => Option[Slot]) = trySolve { grid =>
		grid.trySolve { slot => f(slot).map { newSlot => newSlot -> newSlot } } }.map { case (newState, changes) =>
		newState -> changes.flatten }
	
	/**
	 * Tries to solve the grids in this sudoku using specified mapping function
	 * @param f A mapping function that includes made changes. Returns none if there was no change.
	 * @return Mapped version of this sudoku + whether any change was made
	 */
	def trySolveGrids(f: Grid => Option[(Grid, Vector[Slot])]) = trySolve { grid =>
		f(grid).map { case (newGrid, modifiedSlots) => newGrid -> (newGrid -> modifiedSlots) } }
	
	/**
	 * Tries to solve this sudoku by targeting each line on the specified axis
	 * @param axis Target axis
	 * @param f A solve function that targets lines and returns a) new version of line + b) changed slots within
	 *          that line. Returns none if there was no change.
	 * @return A new version of this sudoku + list of changed lines, along with their changed slots. None if there was no change.
	 */
	def trySolveLinesAlong(axis: Axis2D)(f: SlotLine => Option[(SlotLine, Vector[Slot])]) =
	{
		trySolve(slotLinesAlong(axis).toVector) { line => f(line).map { case (newLine, modifiedSlots) =>
			newLine -> (newLine -> modifiedSlots) } } { newLines => withSlotLines(axis, newLines) }
	}
	
	/**
	 * Tries to solve this sudoku by targeting each row
	 * @param f A solve function that targets rows and returns a) new version of row + b) changed slots within
	 *          that row. Returns none if there was no change.
	 * @return A new version of this sudoku + list of changed rows, along with their changed slots. None if there was no change.
	 */
	def trySolveRows(f: SlotLine => Option[(SlotLine, Vector[Slot])]) = trySolveLinesAlong(X)(f)
	
	/**
	 * Tries to solve this sudoku by targeting each column
	 * @param f A solve function that targets columns and returns a) new version of column + b) changed slots within
	 *          that column. Returns none if there was no change.
	 * @return A new version of this sudoku + list of changed columns, along with their changed slots. None if there was no change.
	 */
	def trySolveColumns(f: SlotLine => Option[(SlotLine, Vector[Slot])]) = trySolveLinesAlong(Y)(f)
	
	private def withSlotLines(axis: Axis2D, newLines: Vector[SlotLine]) =
	{
		val gridsPerLine = sideLength
		val lineSegmentsPerGrid = newLines.size / sideLength
		val splitLengthLines = newLines.map { _.grouped(lineSegmentsPerGrid).toVector }
		
		val linesGrid = splitLengthLines.grouped(gridsPerLine).toVector
		val newGrids = linesGrid.map { gridsLine =>
			lineIndices.map { d2 =>
				val gridLines = gridsLine.map { _(d2) }
				Grid.fromLines(gridLines, axis)
			}
		}
		SudokuState.fromLines(newGrids, axis)
	}
}