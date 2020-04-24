package sudoku.controller

import utopia.flow.util.CollectionExtensions._
import sudoku.model.{FullSlotsGroup, Slot, SolvableGroupType}

object FindHalfPlaces
{
	/**
	 * Variants of this algorithm that each target a different slot group type
	 */
	val variants = SolvableGroupType.values.map { new FindHalfPlaces(_) }
}

/**
 * Marks places where a number can only go to two different places in a group
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
class FindHalfPlaces(override val targetType: SolvableGroupType) extends SolveNextGroup
{
	// ATTRIBUTES	----------------------------
	
	private implicit val languageCode: String = "en"
	
	
	// IMPLEMENTED	----------------------------
	
	override protected def solve[G <: FullSlotsGroup[G]](group: G) =
	{
		val missingNumbers = group.missingNumbers
		val emptySlots = group.emptySlots
		// Finds cases where there are exactly 2 positions a number can go
		val assignments = missingNumbers.flatMap { number =>
			val possibleSlots = emptySlots.filter { _.availableNumbers.contains(number) }
			if (possibleSlots.size == 2)
				Vector(possibleSlots.head -> number, possibleSlots(1) -> number)
			else
				None
		}
		
		// Assigns the numbers to slots, creating a new group
		if (assignments.nonEmpty)
		{
			val assignmentsPerSlot = assignments.toVector.asMultiMap
			group.trySolveMap { slot =>
				assignmentsPerSlot.get(slot) match
				{
					case Some(numbers) => slot.withHalfPlaces(targetType, numbers.toSet)
					case None => slot
				}
			}
		}
		else
			None
	}
	
	// Affecting slots are always empty
	override protected def affectingSlots(from: FullSlotsGroup[_], modifiedSlots: Set[Slot]) = Set()
	
	override def name = s"Finds places where number can only fit 2 locations in a ${targetType.name}"
}
