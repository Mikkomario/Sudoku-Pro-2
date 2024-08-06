package vf.sudoku.model.grid

import utopia.paradigm.enumeration.Axis.{X, Y}
import utopia.paradigm.enumeration.Axis2D
import vf.sudoku.model.solve.Solvable

/**
 * A common trait for objects used for creating grids
 * @author Mikko Hilpinen
 * @since 23.4.2020, v1
 */
trait GridFactory[A <: Solvable, +G <: GridLike[A, G]]
{
	// ABSTRACT	---------------------------
	
	/**
	 * Creates a new grid from specified items (will be placed in a grid-like manner)
	 * @param items Items to place on grid (left to right, top to bottom)
	 * @return A new grid
	 */
	def apply(items: Vector[A]): G
	
	
	// OTHER	---------------------------
	
	/**
	 * @param rows Rows to place on a grid (row length should be equal to column length)
	 * @return A new grid with specified rows
	 */
	def fromRows(rows: Vector[IterableOnce[A]]) = apply(rows.flatten)
	
	/**
	 * @param columns Columns to place on a grid (column length must be equal to row length)
	 * @return A new grid with specified columns
	 */
	def fromColumns(columns: Seq[Seq[A]]) =
	{
		// Purposefully throws on asymmetric grids
		apply(columns.indices.flatMap { rowIndex =>
			columns.map { _(rowIndex) }
		}.toVector)
	}
	
	/**
	 * @param lines Lines that form this grid
	 * @param axis Axis that determines line alignment
	 * @return A new grid
	 */
	def fromLines(lines: Vector[Seq[A]], axis: Axis2D) = axis match
	{
		case X => fromRows(lines)
		case Y => fromColumns(lines)
	}
}
