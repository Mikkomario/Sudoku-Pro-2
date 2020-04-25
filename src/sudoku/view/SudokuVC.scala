package sudoku.view

import sudoku.model.{BorderSettings, Position, Slot, SudokuState}
import utopia.flow.datastructure.immutable.Graph.GraphViewNode
import utopia.flow.datastructure.mutable.PointerWithEvents
import utopia.flow.util.TimeExtensions._
import utopia.genesis.color.Color
import utopia.genesis.handling.{Actor, MouseButtonStateListener}
import utopia.genesis.shape.Vector3D
import utopia.genesis.shape.shape2D.{Bounds, Line}
import utopia.genesis.util.Drawer
import utopia.inception.handling.HandlerType
import utopia.reflection.component.RefreshableWithPointer
import utopia.reflection.component.drawing.mutable.CustomDrawableWrapper
import utopia.reflection.component.drawing.template.CustomDrawer
import utopia.reflection.component.drawing.template.DrawLevel.Background
import utopia.reflection.component.swing.StackableAwtComponentWrapperWrapper
import utopia.reflection.controller.data.ContainerContentManager
import utopia.reflection.shape.Margins
import utopia.reflection.util.ComponentContextBuilder

import scala.concurrent.duration.FiniteDuration

object SudokuVC
{
	private val highlightDuration = 3.seconds
	private val numberHighlightChangeDuration = 0.25.seconds
}

/**
 * Represents a sudoku state visually
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
class SudokuVC(initialState: SudokuState)(implicit baseCB: ComponentContextBuilder, margins: Margins, borderSettings: BorderSettings)
	extends StackableAwtComponentWrapperWrapper with RefreshableWithPointer[SudokuState] with CustomDrawableWrapper
{
	import SudokuVC._
	
	// ATTRIBUTES	----------------------------
	
	val contentPointer = new PointerWithEvents[SudokuState](initialState)
	
	private var highlightedNumber: Option[Int] = None
	private var lastHighlightedNumber = 1
	
	private val container = new GridContainer[GridVC]
	private val manager = ContainerContentManager.forImmutableStates(container, initialState.grids) {
		_.position == _.position } { grid => new GridVC(grid) }
	
	private var _lastModifiedSlots = Set[Slot]()
	private var _relatedSlots = Set[Slot]()
	private var remainingHighlightLevel = 0.0
	
	private var numberHighlightLevel = 0.0
	
	private var currentlySelectedSlot: Option[Slot] = None
	private var currentLinksNode: Option[GraphViewNode[Slot, Int]] = None
	
	
	// COMPUTED	-------------------------------
	
	private def gridDisplays = container.components
	private def slotDisplaysView = gridDisplays.view.flatMap { _.slotDisplays }
	
	private def isHighlighted = remainingHighlightLevel > 0
	
	
	// INITIAL CODE	---------------------------
	
	contentPointer.addListener { e => manager.content = e.newValue.grids }
	addCustomDrawer(ModifiedSlotsHighlighter)
	addCustomDrawer(NumberHighlighter)
	addCustomDrawer(LinksDrawer)
	baseCB.actorHandler += HighLightUpdater
	baseCB.actorHandler += NumberHighLightUpdater
	
	addMouseButtonListener(MouseButtonStateListener.onLeftPressed { e =>
		// Finds the slot that was pressed
		val positionInContainer = e.positionOverArea(container.bounds)
		container.components.find { _.bounds.contains(positionInContainer) }.flatMap { grid =>
			val positionInGrid = positionInContainer - grid.position
			grid.slotDisplays.find { _.bounds.contains(positionInGrid) }
		} match
		{
			case Some(clickedSlotVC) =>
				println(s"Clicked on ${clickedSlotVC.content.position}")
				val slot = clickedSlotVC.content
				if (currentlySelectedSlot.contains(slot))
					currentlySelectedSlot = None
				else
				{
					currentLinksNode = Some(content.halfPairsGraph.all(slot))
					currentlySelectedSlot = Some(slot)
				}
			case None =>
				println("Clicked outside of slots")
				currentlySelectedSlot = None
		}
		repaint()
		None
	})
	
	
	// IMPLEMENTED	----------------------------
	
	override def drawable = container
	
	override protected def wrapped = container
	
	
	// OTHER	--------------------------------
	
	/**
	 * Starts highlighting specified number
	 * @param number Number to be highlighted
	 */
	def highlightNumber(number: Int) = highlightedNumber = Some(number)
	
	/**
	 * Ends highlighting the specified number
	 * @param number Number to be no longer highlighted
	 */
	def endHighlightOfNumber(number: Int) =
	{
		if (highlightedNumber.contains(number))
		{
			lastHighlightedNumber = number
			highlightedNumber = None
		}
	}
	
	/**
	 * Highlights the specified slots in this view
	 * @param modifiedSlots Slots that were recently modified
	 * @param relatedSlots Slots that related to modifying
	 */
	def highlight(modifiedSlots: Set[Slot], relatedSlots: Set[Slot]) =
	{
		_lastModifiedSlots = modifiedSlots
		_relatedSlots = relatedSlots
		remainingHighlightLevel = 1.0
	}
	
	
	// NESTED	--------------------------------
	
	private object HighLightUpdater extends Actor
	{
		override def act(duration: FiniteDuration) =
		{
			val highlightMod = duration / highlightDuration
			remainingHighlightLevel -= highlightMod
			repaint()
		}
		
		override def allowsHandlingFrom(handlerType: HandlerType) = isHighlighted
	}
	
	private object NumberHighLightUpdater extends Actor
	{
		override def act(duration: FiniteDuration) =
		{
			val adjustment = duration / numberHighlightChangeDuration
			if (highlightedNumber.isDefined)
				numberHighlightLevel = (numberHighlightLevel + adjustment) min 1
			else
				numberHighlightLevel = (numberHighlightLevel - adjustment) max 0
			
			repaint()
		}
		
		override def allowsHandlingFrom(handlerType: HandlerType) =
		{
			if (highlightedNumber.isDefined)
				numberHighlightLevel < 1
			else
				numberHighlightLevel > 0
		}
	}
	
	private object ModifiedSlotsHighlighter extends CustomDrawer
	{
		// ATTRIBUTES	------------------------
		
		private val modifiedColor = Color.green.timesSaturation(0.5)
		private val relatedColor = Color.yellow.timesSaturation(0.5)
		
		
		// IMPLEMENTED	------------------------
		
		override def drawLevel = Background
		
		override def draw(drawer: Drawer, bounds: Bounds) =
		{
			if (isHighlighted)
			{
				// Goes through each modified slot and draws a background for it
				highlightSlots(drawer, _lastModifiedSlots, modifiedColor)
				highlightSlots(drawer, _relatedSlots, relatedColor)
			}
		}
		
		
		// OTHER	----------------------------
		
		private def highlightSlots(drawer: Drawer, slots: Set[Slot], color: Color) =
		{
			if (slots.nonEmpty)
				drawer.onlyFill(color.timesAlpha(remainingHighlightLevel)).disposeAfter { d =>
					gridDisplays.foreach { gridVC =>
						gridVC.slotDisplays.filter { slotVC => slots.contains(slotVC.content) }.foreach { slotVC =>
							val bounds = slotVC.bounds + gridVC.position
							d.draw(bounds)
						}
					}
				}
		}
	}
	
	private object NumberHighlighter extends CustomDrawer
	{
		// ATTRIBUTES	-------------------------
		
		private val possibleColor = Color.blue.timesSaturation(0.5).withAlpha(0.33)
		private val takenColor = Color.red.timesSaturation(0.5).withAlpha(0.55)
		
		
		// IMPLEMENTED	-------------------------
		
		override def drawLevel = Background
		
		override def draw(drawer: Drawer, bounds: Bounds) =
		{
			if (numberHighlightLevel > 0)
			{
				val number = highlightedNumber.getOrElse(lastHighlightedNumber)
				
				// Draws the possible slots, then the taken slots
				highlightSlots(drawer, possibleColor) { slot => slot.nonSolved && slot.availableNumbers.contains(number) }
				highlightSlots(drawer, takenColor) { _.number.contains(number) }
			}
		}
		
		
		// OTHER	-----------------------------
		
		private def highlightSlots(drawer: Drawer, color: Color)(filter: Slot => Boolean) =
		{
			drawer.onlyFill(color.timesAlpha(numberHighlightLevel)).disposeAfter { d =>
				gridDisplays.foreach { gridVC =>
					gridVC.slotDisplays.filter { slotVC => filter(slotVC.content) }.foreach { slotVC =>
						val bounds = slotVC.bounds + gridVC.position
						d.draw(bounds)
					}
				}
			}
		}
	}
	
	private object LinksDrawer extends CustomDrawer
	{
		val color = Color.blue.withAlpha(0.22)
		
		override def drawLevel = Background
		
		override def draw(drawer: Drawer, bounds: Bounds) =
		{
			// Draws a line from each link to the next step
			currentLinksNode.foreach { originNode =>
				drawer.onlyEdges(Color.blue).withStroke(3).disposeAfter { d =>
					originNode.foreach { node =>
						val origin = slotPositionToPixels(node.content.position, bounds)
						node.leavingEdges.foreach { edge =>
							val target = slotPositionToPixels(edge.end.content.position, bounds)
							d.draw(Line(origin, target))
						}
					}
				}
			}
		}
		
		def slotPositionToPixels(position: Position, bounds: Bounds) =
		{
			bounds.position + (Vector3D(position.x, position.y) / 9 * bounds.size) + bounds.size / 18
		}
	}
}
