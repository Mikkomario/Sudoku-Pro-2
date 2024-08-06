package vf.sudoku.model.grid

import scala.language.implicitConversions

object SlotLine
{
	// Auto-converts vectors to slot lines
	implicit def vectorToSlotLine(vector: Vector[Slot]): SlotLine = SlotLine(vector)
}

/**
 * A line of slots
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
case class SlotLine(slots: Vector[Slot]) extends Line[Slot](slots) with FullSlotsGroup[SlotLine]
{
	override def withItems(newSlots: Vector[Slot]) = copy(slots = newSlots)
	
	override def repr = this
}
