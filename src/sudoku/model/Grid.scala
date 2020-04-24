package sudoku.model

import utopia.flow.util.CollectionExtensions._

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
		rowStrings.paired.flatMap { case (first, _) => Vector(first, rowSeparator) } :+ rowStrings.last
	}
	
	
	// IMPLEMENTED	---------------------------
	
	override def withItems(newItems: Vector[Slot]) = copy(slots = newItems)
	
	override def items = slots
	
	override def repr = this
}
