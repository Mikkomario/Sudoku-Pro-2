package sudoku.model

import sudoku.model.SolvableGroupType.{Column, Row}
import sudoku.util.MultiMapBuilder
import utopia.flow.datastructure.immutable.Graph
import utopia.flow.datastructure.immutable.Graph.GraphViewNode
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
			// Targets each slot in each grid separately (ignores solved slots)
			grid.emptySlots.foreach { slot =>
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
	
	/**
	 * Includes only those slots that have exactly 2 possible numbers that can be placed in them
	 */
	lazy val twins = all.filterByNodeContent { _.availableNumbers.size == 2 }
	
	
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
	 * @return All chains created by linked twin slots, grouped into individual sub-graphs. Chains within a sub-graph
	 *         are interconnected and affect each other. The returned chains contain each chain <b>twice</b>, each
	 *         time with different direction.
	 */
	def twinChains =
	{
		// First finds all the chains / subgraphs within the twins
		twins.subGraphs.map { chainGraph =>
			// Checks if there's a node where the chain starts (only has 1 connection)
			// If there was such a node, starts the chains from that one
			// Otherwise starts the chains from an arbitrary node
			val startNode = chainGraph.nodes.find { _.leavingEdges.map { _.end }.size == 1 }.getOrElse(chainGraph.nodes.head)
			// Registers all possible chains that include this node
			startNode.toTreeWithoutEdges.allBranches.map { chain => startNode.content +: chain }
		}
	}
	
	
	// OTHER	-----------------------------
	
	def forGroupType(groupType: SolvableGroupType) = graphsByCategory.getOrElse(groupType,
		Graph.empty(isTwoWayBound = true))
	
	/**
	 * @param slot A slot
	 * @return Tree that represents the linked slots
	 */
	def chainsFrom(slot: Slot) = chainsFromNode(all(slot))
	
	/**
	 * @param slot A slot
	 * @return Tree that represents linked twin slots
	 */
	def twinChainsFrom(slot: Slot) = chainsFromNode(twins(slot))
	
	private def chainsFromNode(node: GraphViewNode[Slot, _]) =
		node.toTreeWithoutEdges.allBranches.map { branch => node.content +: branch }
}
