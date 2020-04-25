package sudoku.controller

import sudoku.model.{FullSlotsGroup, Slot, SolvableGroupType}

object NakedTwinsRule extends GroupSolveFactory[NakedTwinsRule]
{
	override def apply(targetGroupType: SolvableGroupType) = new NakedTwinsRule(targetGroupType)
}

/**
 * Checks for "naked pairs" in a slot group. A naked pair is a case where there are two slots that can only hold the
 * same two numbers. Therefore, the number can only be in one of those places, restricting it from other possible
 * slots in the group.
 * @author Mikko Hilpinen
 * @since 25.4.2020, v1
 */
class NakedTwinsRule(override val targetType: SolvableGroupType) extends SolveNextGroup
{
	// ATTRIBUTES	----------------------------
	
	private implicit val languageCode: String = "en"
	
	override val name = "Naked twin rule"
	
	
	// IMPLEMENTED	----------------------------
	
	override protected def solve[G <: FullSlotsGroup[G]](group: G) =
	{
		// Checks whether the group contains any naked pairs (needs to have two slots with exactly same pairing numbers)
		// Those slots are found among those that only have 2 possible numbers left
		val twins = nakedTwins(group)
		
		if (twins.nonEmpty)
		{
			// The numbers will be limited to the found twins
			// Goes through each slot and applies the new restrictions
			group.trySolveFlatMap { slot =>
				val restrictedNumbers = twins.filterNot { _._2.contains(slot) }.flatMap { _._1 }
				if (restrictedNumbers.nonEmpty)
					Some(slot.withNotAllowed(restrictedNumbers))
				else
					None
			}
		}
		else
			None
	}
	
	override protected def affectingSlots(from: FullSlotsGroup[_], modifiedSlots: Set[Slot]) =
	{
		// Returns naked twins of the group
		nakedTwins(from).flatMap { _._2 }
	}
	
	
	// OTHER	--------------------------------
	
	// [Pair -> The naked twins for that pair]
	private def nakedTwins(group: FullSlotsGroup[_]) =
	{
		val onlyTwoNumberSlots = group.emptySlots.filter { _.availableNumbers.size == 2 }
		val uniquePairs = onlyTwoNumberSlots.map { _.availableNumbers }.toSet
		// Checks which number pairs appear twice in the group
		val doublePairs = uniquePairs.filter { pair => onlyTwoNumberSlots.count { _.availableNumbers == pair } > 1 }
		// Returns the slots containing those pairs
		doublePairs.map { pair => pair -> onlyTwoNumberSlots.filter { _.availableNumbers == pair } }
	}
}
