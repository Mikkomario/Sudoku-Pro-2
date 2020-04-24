package sudoku.controller

import sudoku.model.{FullSlotsGroup, Slot, SolvableGroupType}

object RemoveDuplicates
{
	/**
	 * All possible variants of this algorithm (each targets different segments of the sudoku)
	 */
	val variants = SolvableGroupType.values.map { new RemoveDuplicates(_) }
}

/**
 * This very simple algorithm limits single grid values to non-duplicates
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
class RemoveDuplicates(override val targetType: SolvableGroupType) extends SolveAllGroups
{
	// ATTRIBUTES	----------------------
	
	implicit val languageCode: String = "en"
	
	
	// IMPLEMENTED	----------------------
	
	override protected def solve[G <: FullSlotsGroup[G]](group: G) =
		group.trySolveMap { _.withNotAllowed(group.numbers) }
	
	// Lists specified slots within target groups
	override protected def affectingSlots(from: Vector[FullSlotsGroup[_]], modifiedSlots: Set[Slot]) =
		from.flatMap { _.filledSlots }.toSet -- modifiedSlots
	
	override val name = s"A ${targetType.name} can only have a single instance of each number"
}
