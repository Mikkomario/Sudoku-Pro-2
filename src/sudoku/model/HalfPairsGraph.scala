package sudoku.model

import sudoku.model.SolvableGroupType.{Column, Row}
import sudoku.util.MultiMapBuilder
import utopia.flow.datastructure.immutable.Graph
import utopia.flow.util.CollectionExtensions._

import scala.collection.mutable

object HalfPairsGraph
{
	/**
	 * Creates a half-pairs graph based on slot grids
	 * @param grids Grids
	 * @return A graph based on the grids
	 */
	def apply(grids: Iterable[Grid]) =
	{
		// Goes through the slots and builds half-number + slot lists for each row, column and grid
		// GroupType -> [Group index -> [Number -> [Slots]]]
		val builder = SolvableGroupType.values.map { category => (category: SolvableGroupType) ->
			mutable.Map[Int, MultiMapBuilder[Int, Slot]]() }.toMap
		
		// Goes through each grid
		grids.zipWithIndex.foreach { case (grid, gridIndex) =>
			// Targets each slot in each grid separately
			grid.slots.foreach { slot =>
				// Handles each half-number style (row, column, grid) separately
				slot.halfPlaceFor.foreach { case (category, numbers) =>
					val indexInCategory = category match
					{
						case SolvableGroupType.Grid => gridIndex
						case Row => slot.position.y
						case Column => slot.position.x
					}
					// Adds a new builder for this group if necessary
					val numbersBuilder = builder(category).getOrElseUpdate(indexInCategory, new MultiMapBuilder)
					// Registers this slot for the half-numbers
					numbers.foreach { number => numbersBuilder += number -> slot }
				}
			}
		}
		
		// Finalizes the built structure
		val groupedNumberSlots = builder.view.mapValues { numbersByIndex =>
			numbersByIndex.view.mapValues { numbersBuilder => numbersBuilder.result() }.toMap
		}.toMap
		
		val graphsByCategory = groupedNumberSlots.map { case (category, linksByIndex) =>
			// There's an edge between each same-number slots in a shared index
			val links = linksByIndex.values.flatMap { _.flatMap { case (number, slots) =>
				slots.paired.map { case (slot1, slot2) => (slot1, number, slot2) }
			} }.toSet
			// Creates a graph based on each category of links
			category -> Graph(links, isTwoWayBound = true)
		}
		new HalfPairsGraph(graphsByCategory)
	}
}

/**
 * A graph representation of half-number connections between slots. A half-number connection is created when a number
 * in a row, column or a grid can only be placed in two slots.
 * @author Mikko Hilpinen
 * @since 25.4.2020, v1
 */
case class HalfPairsGraph private(private val graphsByCategory: Map[SolvableGroupType, Graph[Slot, Int]])
{
	// ATTRIBUTES	--------------------------
	
	/**
	 * All registered pairs as a graph
	 */
	val all = graphsByCategory.values.reduceOption { _ ++ _ }.getOrElse(Graph.empty(isTwoWayBound = true))
	
	/**
	 * Includes only those slots that have two connections to a direction
	 */
	lazy val pairs = all.filterByNodeContent { _.halfPlaceFor.values.exists { _.size > 1 } }
	
	
	// COMPUTED	-----------------------------
	
	def forRows = forGroupType(Row)
	
	def forColumns = forGroupType(Column)
	
	def forGrids = forGroupType(SolvableGroupType.Grid)
	
	/**
	 * @return Closed chains within this graph that have length of 3 or more. Each set of nodes is also paired
	 *         with the chaining half-numbers.
	 */
	def closedChains =
	{
		all.nodes.flatMap { node =>
			// Finds the routes from a node to itself (must be through at least 2 other nodes)
			val routes = node.routesToSelf.filter { _.size > 2 }
			routes.map { route =>
				// Checks which numbers and which slots were traversed
				val numbers = route.map { _.content }.toSet
				val slots = route.map { _.end.content }.toSet
				slots -> numbers
			}
		}
	}
	
	/**
	 * @param slot A slot
	 * @return Tree that represents the linked slots
	 */
	def chainsFrom(slot: Slot) = all(slot).toTreeWithoutEdges
	
	
	// OTHER	-----------------------------
	
	def forGroupType(groupType: SolvableGroupType) = graphsByCategory.getOrElse(groupType,
		Graph.empty(isTwoWayBound = true))
}
