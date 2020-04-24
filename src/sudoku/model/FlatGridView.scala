package sudoku.model

import utopia.genesis.shape.Axis.{X, Y}
import utopia.genesis.shape.Axis2D

/**
 * Views a grid of grids as a larger grid of the internal grids' items
 * @author Mikko Hilpinen
 * @since 23.4.2020, v1
 */
class FlatGridView[A <: Solvable, G <: GridLike[A, G], +GG <: GridLike[G, GG]]
(grid: GG)(makeCopy: Vector[A] => GridLike[A, _]) extends GridLike[A, GridLike[A, _]]
{
	// ATTRIBUTES	--------------------------
	
	override val rows = _lines(X)
	
	override lazy val columns = _lines(Y)
	
	
	// IMPLEMENTED	--------------------------
	
	override def repr = this
	
	override def withItems(newItems: Vector[A]): GridLike[A, _] = makeCopy(newItems)
	
	override def items = rows.flatten
	
	override val sideLength = rows.head.size
	
	
	// OTHER	------------------------------
	
	private def _lines(axis: Axis2D) =
	{
		grid.lines(axis).flatMap { gridLine =>
			val linesPerGrid = gridLine.map { _.lines(axis) }
			val numberOfLinesInGridLine = linesPerGrid.map { _.size }.min
			(0 until numberOfLinesInGridLine).map { lineIndex => linesPerGrid.flatMap { _(lineIndex) }.toVector }
		}.toVector
	}
}
