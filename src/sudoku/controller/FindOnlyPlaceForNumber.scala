package sudoku.controller
import sudoku.model.{FullSlotsGroup, Slot, SolvableGroupType}

object FindOnlyPlaceForNumber
{
	/**
	 * Variants of this algorithm that target different slot groups
	 */
	val variants = SolvableGroupType.values.map {new FindOnlyPlaceForNumber(_) }
}

/**
 * This algorithm checks whether a number can only placed within a single location in a numbers group
 * @since 24.4.2020, v1
 */
class FindOnlyPlaceForNumber(override val targetType: SolvableGroupType) extends SolveNextGroup
{
	private implicit val languageCode: String = "en"
	
	override def name = s"Finds the only place a number can go in a ${targetType.name}"
	
	override protected def solve[G <: FullSlotsGroup[G]](group: G) =
	{
		// Checks if a number can only be assigned to a single place, records slot number pairs
		val emptySlots = group.emptySlots
		val numbersToAssign = group.missingNumbers.flatMap { number =>
			val availableSlots = emptySlots.filter { _.availableNumbers.contains(number) }
			if (availableSlots.size == 1)
				Some(availableSlots.head -> number)
			else
				None
		}.toMap
		
		// Creates a new copy of the group, based on the findings (if suitable numbers were found)
		if (numbersToAssign.isEmpty)
			None
		else
		{
			group.trySolve { slot => numbersToAssign.get(slot).map { number =>
				val newSlot = slot.copy(number = Some(number))
				newSlot -> newSlot
			} }
		}
	}
	
	// Counts other empty slots in the group
	override protected def affectingSlots(from: FullSlotsGroup[_], modifiedSlots: Set[Slot]) = from.emptySlots.toSet -- modifiedSlots
}
