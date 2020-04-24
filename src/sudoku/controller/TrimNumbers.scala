package sudoku.controller

import sudoku.model.{FullSlotsGroup, Slot, SolvableGroupType}

object TrimNumbers
{
	/**
	 * Variants of this algorithm for each group type
	 */
	val variants = SolvableGroupType.values.map { new TrimNumbers(_) }
}

/**
 * Fills in required numbers to fill the group, but also limits the slot numbers to those available in the group
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
class TrimNumbers(override val targetType: SolvableGroupType) extends SolveAllGroups
{
	// ATTRIBUTES	----------------------
	
	implicit val languageCode: String = "en"
	
	
	// IMPLEMENTED	----------------------
	
	override protected def solve[G <: FullSlotsGroup[G]](group: G) =
	{
		val existingNumbers = group.numbers
		val missingNumbers = group.missingNumbers
		group.trySolveMap { slot => slot.withNotAllowed(existingNumbers).withMustBe(missingNumbers) }
	}
	
	// Lists specified slots within target groups
	override protected def affectingSlots(from: Vector[FullSlotsGroup[_]], modifiedSlots: Set[Slot]) =
		from.flatMap { _.filledSlots }.toSet -- modifiedSlots
	
	override val name = s"A ${targetType.name} must have exactly one instance of each number"
}
