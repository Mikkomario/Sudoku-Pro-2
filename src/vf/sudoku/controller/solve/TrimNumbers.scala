package vf.sudoku.controller.solve

import vf.sudoku.model.grid.{FullSlotsGroup, Slot}
import vf.sudoku.model.solve.{SolvableGroupType, SolveResult, SudokuState}

import scala.collection.immutable.VectorBuilder

object TrimNumbers extends SolveAlgorithm
{
	private implicit val languageCode: String = "en"
	
	/**
	 * Variants of this algorithm for each group type
	 */
	val variants = SolvableGroupType.values.map { new TrimNumbers(_) }
	
	override def name = "Makes sure that grids, rows and columns have only one instance of each number"
	
	override def apply(sudoku: SudokuState) =
	{
		val modifiedSlotsBuilder = new VectorBuilder[Slot]
		val affectingSlotsBuilder = new VectorBuilder[Slot]
		var foundSuccess = false
		
		val finalState = variants.foldLeft(sudoku) { (problem, solver) =>
			val result = solver(problem)
			if (result.wasSuccess)
			{
				foundSuccess = true
				modifiedSlotsBuilder ++= result.modifiedSlots
				affectingSlotsBuilder ++= result.relatedSlots
			}
			result.newState
		}
		
		SolveResult(foundSuccess, finalState, modifiedSlotsBuilder.result().toSet, affectingSlotsBuilder.result().toSet, Some(name))
	}
}

/**
 * Fills in required numbers to fill the group, but also limits the slot numbers to those available in the group
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
class TrimNumbers(override val targetType: SolvableGroupType) extends SolveAllGroups
{
	import TrimNumbers.languageCode
	
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
