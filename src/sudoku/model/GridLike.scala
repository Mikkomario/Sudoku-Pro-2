package sudoku.model

import utopia.paradigm.enumeration.Axis.{X, Y}
import utopia.paradigm.enumeration.Axis2D

/**
 * Represents a grid-like collection of items
 * @author Mikko Hilpinen
 * @since 23.4.2020, v1
 */
trait GridLike[A <: Solvable, +Repr] extends SolvableGroup[A, Repr]
{
	// ABSTRACT	-----------------------------
	
	/**
	 * @return Length of a single "side" in this grid (the number of items in row / column)
	 */
	def sideLength: Int
	
	
	// COMPUTED	------------------------------
	
	/**
	 * @return The number of items within this grid like element
	 */
	def numberOfItems = math.pow(sideLength, 2)
	
	/**
	 * @return Indices for rows or columns
	 */
	def lineIndices = 0 until sideLength
	
	/**
	 * @return All rows in this grid, from top to bottom
	 */
	def rows = lineIndices.map(row)
	
	/**
	 * @return All columns in this grid, from left to right
	 */
	def columns = lineIndices.map(column)
	
	
	// OTHER	------------------------------
	
	/**
	 * @param axis Line axis
	 * @return All lines in this grid that align with specified axis
	 */
	def lines(axis: Axis2D) = lineIndices.map { index => line(index, axis) }
	
	/**
	 * @param index Index of the row where 0 is the first row
	 * @return Slots in the specified row
	 */
	def row(index: Int) = items.slice(index * sideLength, index * sideLength + sideLength)
	
	/**
	 * @param index Index of the column where 0 is the first column
	 * @return Slots in the specified column
	 */
	def column(index: Int) = (index until items.size by sideLength).map { items(_) }
	
	/**
	 * @param index Index of the row/column
	 * @param axis X if targeting rows, Y if targeting columns
	 * @return All items in this grid along the specified line at index 'index'
	 */
	def line(index: Int, axis: Axis2D) = axis match
	{
		case X => row(index)
		case Y => column(index)
	}
}
