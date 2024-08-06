package vf.sudoku.controller.solve

import utopia.flow.collection.CollectionExtensions._
import vf.sudoku.model.grid.Slot
import vf.sudoku.model.solve.{SolveResult, SudokuState}
import vf.sudoku.util.MultiMapBuilder

/**
 * This algorithm operates over sudoku slot graphs and targets slots associated with number pair chain links
 * @author Mikko Hilpinen
 * @since 26.4.2020, v1
 */
object ChainAssociationsExclusion extends SolveAlgorithm
{
	// ATTRIBUTES	---------------------
	
	private implicit val languageCode: String = "en"
	
	override def name = "Trims numbers associated with open chains"
	
	override def apply(sudoku: SudokuState) =
	{
		// Finds the chains within the sudoku that consist of 2-number pairs (AB, BC, CA and longer versions of that)
		// The chains don't need to be closed (= AB and CA don't have to be connected)
		val twinChainGroups = sudoku.halfPairsGraph.twinChains
		if (twinChainGroups.nonEmpty)
		{
			// Collected format: Slot -> [numbers it cannot have]
			val restrictionsBuilder = new MultiMapBuilder[Slot, Int]
			// Also collects important slot associations (slot -> [affecting slots])
			val affectedSlotsBuilder = new MultiMapBuilder[Slot, Slot]
			val associations = sudoku.groupAssociationsGraph
			
			// Handles each twin chain group individually
			twinChainGroups.foreach { chainGroup =>
				// Only considers unique chains, disregarding ordering
				val twinChains = chainGroup.map { _.toSet }.toSet
				// Slots that are part of this chain group cannot be altered by this chain
				val protectedSlots = twinChains.flatten
				
				// Tests each slot that is associated with two links in a same chain. If the slot is associated with both,
				// it cannot contain the common number between those links
				twinChains.foreach { chain =>
					chain.toVector.paired.foreach { slots =>
						slots.mapAndMerge { _.availableNumbers } { _ & _ }.foreach { commonNumber =>
							// Will, of course, not touch the chain slots themselves
							(associations.slotsAssociatedWithAll(slots) -- protectedSlots).filterNot { _.cantBe.contains(commonNumber) }
								.foreach { slot =>
									restrictionsBuilder += slot -> commonNumber
									affectedSlotsBuilder ++= slot -> slots
								}
						}
					}
				}
			}
			
			val restrictionsPerSlot = restrictionsBuilder.result()
			if (restrictionsPerSlot.nonEmpty)
			{
				sudoku.trySolveSlots { slot =>
					restrictionsPerSlot.get(slot).map { restrictedNumbers =>
						slot.withNotAllowed(restrictedNumbers.toSet)
					}
				} match
				{
					case Some(result) =>
						val (newState, slots) = result
						val affectingSlots = affectedSlotsBuilder.result().view.filterKeys(slots.contains).flatMap { _._2 }.toSet
						SolveResult.success(newState, slots.toSet, affectingSlots, Some(name))
					case None => SolveResult.failure(sudoku)
				}
			}
			else
				SolveResult.failure(sudoku)
		}
		else
			SolveResult.failure(sudoku)
	}
}
