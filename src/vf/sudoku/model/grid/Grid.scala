package vf.sudoku.model.grid

import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair

object Grid extends GridFactory[Slot, Grid]

/**
 * A grid contains 9 sudoku slots
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
case class Grid(slots: Vector[Slot]) extends GridLike[Slot, Grid] with FullSlotsGroup[Grid]
{
	// ATTRIBUTES	----------------------------
	
	override val sideLength = 3
	
	
	// COMPUTED	--------------------------------
	
	/**
	 * @return Position of the top left corner of this grid (in slot coordinate system)
	 */
	def position = slots.head.position
	
	/**
	 * @return Ascii representation of rows and row separators in this grid, each row as a separate item. No borders included
	 */
	def asciiRows =
	{
		val rowSeparator = Iterator.fill(sideLength)("-").mkString("+")
		val rowStrings = rows.map { row => row.map { _.ascii }.mkString("|") }
		rowStrings.paired.flatMap { case Pair(first, _) => Vector(first, rowSeparator) } :+ rowStrings.last
	}
	
	
	// IMPLEMENTED	---------------------------
	
	override def withItems(newItems: Vector[Slot]) = copy(slots = newItems)
	
	override def items = slots
	
	override def repr = this
	
	
	// OTHER	-------------------------------
	
	/**
	 * Checks whether this grid contains the specified position
	 * @param position A position
	 * @return Whether the position is in this grid
	 */
	def containsPosition(position: Coordinate) = {
		val min = slots.head.position
		val max = slots.last.position
		position.x >= min.x && position.x <= max.x && position.y >= min.y && position.y <= max.y
	}
}
