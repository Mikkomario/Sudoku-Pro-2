package sudoku.model

import sudoku.model.SolvableGroupType.{Column, Row}
import sudoku.util.MultiMapBuilder
import utopia.flow.datastructure.immutable.Graph
import utopia.flow.util.CollectionExtensions._

object NumberAssociationGraph
{
	/**
	 * Creates a new number associations graph
	 * @param grids Grids to scan
	 * @return A graph that contains all associations between the grid nodes
	 */
	def apply(grids: Seq[Grid]) =
	{
		val groupSlotBuilders = SolvableGroupType.values.map { gType => gType -> new MultiMapBuilder[Int, Slot] }.toMap
		val indexIdentifiers = Map[SolvableGroupType, (Slot, Int) => Int](
			SolvableGroupType.Grid -> { (_, gridId) => gridId },
			Row -> { (slot, _) => slot.position.y },
			Column -> { (slot, _) => slot.position.x }
		)
		
		// Iterates through the grid. Groups slots based on grid index, x, and y.
		grids.foreachWithIndex { (grid, gridIndex) =>
			grid.emptySlots.foreach { slot =>
				SolvableGroupType.values.foreach { gType =>
					val id = indexIdentifiers(gType)(slot, gridIndex)
					groupSlotBuilders(gType) += id -> slot
				}
			}
		}
		
		// Creates a graph based on grouped slots by recording a two-way connection between all group slots
		// (one slot will belong to multiple groups so each will have three types of connections)
		val graphsByCategory = groupSlotBuilders.map { case (gType, builder) =>
			(gType: SolvableGroupType) -> Graph.twoWayBound(builder.result().valuesIterator.flatten.toVector.paired.map { case (a, b) =>
				(a, gType: SolvableGroupType, b) }.toSet) }
		
		new NumberAssociationGraph(graphsByCategory)
	}
}

/**
 * This graph contains all standard exclusion relationships (same row, same column, same grid) associations
 * between unsolved slots
 * @author Mikko Hilpinen
 * @since 26.4.2020, v1
 */
case class NumberAssociationGraph private(graphs: Map[SolvableGroupType, Graph[Slot, SolvableGroupType]])
{
	// ATTRIBUTES	--------------------------
	
	val graph = graphs.valuesIterator.reduce { _ ++ _ }
	
	/**
	 * Finds all slots that are somehow associated with the specified target slot
	 * @param targetSlot Target slot
	 * @return All empty nodes that are associated with the specified slot
	 */
	def slotsAssociatedWith(targetSlot: Slot) = graph(targetSlot).endNodes.map { _.content } - targetSlot
	
	/**
	 * Finds all slots that are associated with ALL of the specified slots (excluding the slots themselves)
	 * @param targetSlots Targeted slots
	 * @return A set of slots that are associated with ALL of the specified slots
	 */
	def slotsAssociatedWithAll(targetSlots: Iterable[Slot]): Set[Slot] =
	{
		if (targetSlots.isEmpty)
			Set()
		else
			targetSlots.map(slotsAssociatedWith).reduce { _ & _ } -- targetSlots
	}
	
	/**
	 * @param a A slot
	 * @param b Another slot
	 * @return Slots that are associated with both of the specified slots
	 */
	def slotsAssociatedWithBoth(a: Slot, b: Slot) = slotsAssociatedWithAll(Set(a, b))
}
