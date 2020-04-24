package sudoku.controller

import sudoku.model.{FullSlotsGroup, Slot, SolvableGroupType}

object RuleOutNonHalfPairs
{
	val variants = SolvableGroupType.values.map { new RuleOutNonHalfPairs(_) }
}

/**
 * This algorithm goes through slots that have a half-pair (either or relationship) and rules out the remaining numbers
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
class RuleOutNonHalfPairs(override val targetType: SolvableGroupType) extends SolveNextGroup
{
	// ATTRIBUTES	-----------------------------
	
	private implicit val languageCode: String = "en"
	
	
	// IMPLEMENTED	-----------------------------
	
	override def name = s"Rules out numbers outside of half-pairs in ${targetType.name}"
	
	override protected def solve[G <: FullSlotsGroup[G]](group: G) =
	{
		// If a group contains two slots with matching half-pairs, rules out the other numbers in those pairs
		// (since those two numbers must be in those two slots in some order, therefore filling the slots)
		val halfPairSlots = group.emptySlots.map { slot => slot -> slot.halfPairs }
		// NB: May contain some pairs multiple times
		val allHalfPairs = halfPairSlots.flatMap { _._2 }
		val uniqueHalfPairs = allHalfPairs.toSet
		val doublePairs = uniqueHalfPairs.filter { pair => allHalfPairs.count { _ == pair } > 1 }
		
		if (doublePairs.nonEmpty)
		{
			val halfPairsPerSlot = halfPairSlots.toMap
			group.trySolveMap { slot =>
				halfPairsPerSlot.get(slot) match
				{
					case Some(slotPairs) =>
						slotPairs.find(doublePairs.contains) match
						{
							case Some(doublePair) => slot.withMustBe(doublePair)
							case None => slot
						}
					case None => slot
				}
			}
		}
		else
			None
	}
	
	override protected def affectingSlots(from: FullSlotsGroup[_], modifiedSlots: Set[Slot]) = Set()
}
