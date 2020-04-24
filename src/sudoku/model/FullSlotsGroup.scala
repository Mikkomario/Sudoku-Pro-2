package sudoku.model

/**
 * A common trait for groups of slots that must each have a unique number
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
trait FullSlotsGroup[+Repr] extends SolvableGroup[Slot, Repr]
{
	// COMPUTED	-----------------------------
	
	/**
	 * @return The numbers that have already been placed in this group
	 */
	def numbers = items.flatMap { _.number }.toSet
	
	/**
	 * @return The numbers that are still missing from this group
	 */
	def missingNumbers = (1 to items.size).toSet -- numbers
	
	/**
	 * @return Slots in this grid that have already been filled
	 */
	def filledSlots = items.filter { _.isSolved }
	
	/**
	 * @return Slots in this grid that haven't been filled yet
	 */
	def emptySlots = items.filterNot { _.isSolved }
}
