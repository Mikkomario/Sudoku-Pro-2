package vf.sudoku.controller.solve

import vf.sudoku.model.grid.{FullSlotsGroup, Slot}
import vf.sudoku.model.grid.{FullSlotsGroup, Slot}
import vf.sudoku.model.solve.SolvableGroupType

object FillRequirements
{
	/**
	 * Variants of this algorithm, each targeting different segments of the sudoku
	 */
	val variants = SolvableGroupType.values.map { new FillRequirements(_) }
}

/**
 * This algorithm updates grid slots so that each number must be present in the grid
 * @author Mikko Hilpinen
 * @since 23.4.2020, v1
 */
class FillRequirements(override val targetType: SolvableGroupType) extends SolveAllGroups
{
	private implicit val languageCode: String = "en"
	
	override def name = s"Fills numbers required by ${targetType.name}"
	
	override protected def solve[G <: FullSlotsGroup[G]](group: G) =
	{
		val missingNumbers = group.missingNumbers
		group.trySolveMap { slot => slot.withMustBe(missingNumbers) }
	}
	
	// Lists other filled slots in the groups
	override protected def affectingSlots(from: Vector[FullSlotsGroup[_]], modifiedSlots: Set[Slot]) =
		from.flatMap { _.filledSlots }.toSet -- modifiedSlots
}
