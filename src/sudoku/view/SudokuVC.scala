package sudoku.view

import java.awt.event.KeyEvent

import sudoku.model.{Position, Slot, SudokuState}
import utopia.flow.datastructure.mutable.PointerWithEvents
import utopia.flow.util.TimeExtensions._
import utopia.flow.util.CollectionExtensions._
import utopia.genesis.color.Color
import utopia.genesis.event.{KeyStateEvent, MouseButtonStateEvent}
import utopia.genesis.handling.{Actor, KeyStateListener, MouseButtonStateListener}
import utopia.genesis.shape.Vector3D
import utopia.genesis.shape.shape2D.{Bounds, Line}
import utopia.genesis.util.Drawer
import utopia.inception.handling.HandlerType
import utopia.reflection.component.RefreshableWithPointer
import utopia.reflection.component.context.ColorContext
import utopia.reflection.component.drawing.mutable.CustomDrawableWrapper
import utopia.reflection.component.drawing.template.CustomDrawer
import utopia.reflection.component.drawing.template.DrawLevel.Normal
import utopia.reflection.component.swing.StackableAwtComponentWrapperWrapper
import utopia.reflection.controller.data.ContainerContentManager

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
class SudokuVC(initialState: SudokuState, parentContext: ColorContext)
	extends StackableAwtComponentWrapperWrapper with RefreshableWithPointer[SudokuState] with CustomDrawableWrapper
{
	import SudokuVC._
	import DefaultContext._
	
	// ATTRIBUTES	----------------------------
	
	private val backgroundColor = colorScheme.gray.light
	
	val contentPointer = new PointerWithEvents[SudokuState](initialState)
	
	private var highlightedNumber: Option[Int] = None
	private var lastHighlightedNumber = 1
	
	private val container = new GridContainer[GridVC]
	private val manager = parentContext.inContextWithBackground(backgroundColor).use { implicit context =>
		ContainerContentManager.forImmutableStates(container, initialState.grids) {
			_.position == _.position } { grid => new GridVC(grid) }
	}
	
	private var _lastModifiedSlots = Set[Slot]()
	private var _relatedSlots = Set[Slot]()
	private var remainingHighlightLevel = 0.0
	
	private var numberHighlightLevel = 0.0
	
	private var currentlySelectedSlot: Option[Slot] = None
	private var currentHighlightedLinks = Vector[(Slot, Slot)]()
	private var highlightIsTwinRestricted = false
	
	private var isCtrlPressed = false
	private var saveStates = Vector[(SudokuState, Slot)]()
	
	
	// COMPUTED	-------------------------------
	
	private def gridDisplays = container.components
	private def slotDisplaysView = gridDisplays.view.flatMap { _.slotDisplays }
	
	private def isHighlighted = remainingHighlightLevel > 0
	
	
	// INITIAL CODE	---------------------------
	
	container.background = backgroundColor
	
	contentPointer.addListener { e => manager.content = e.newValue.grids }
	addCustomDrawer(ModifiedSlotsHighlighter)
	addCustomDrawer(NumberHighlighter)
	addCustomDrawer(LinksDrawer)
	parentContext.actorHandler += HighLightUpdater
	parentContext.actorHandler += NumberHighLightUpdater
	
	addKeyStateListener(KeyStateListener(KeyStateEvent.keyFilter(KeyEvent.VK_CONTROL)) { e => isCtrlPressed = e.isDown })
	addKeyStateListener(KeyStateListener.onKeyPressed(KeyEvent.VK_Z) { e =>
		if (e.keyStatus(KeyEvent.VK_CONTROL) && saveStates.nonEmpty)
		{
			val (oldState, cancelSlot) = saveStates.last
			println(s"Cancelling last change: ${cancelSlot.position}")
			content = oldState
			highlight(Set(cancelSlot), Set())
			// FIXME: Doesn't draw cancelled slot data correctly!
			saveStates = saveStates.dropRight(1)
		}
	})
	
	addMouseButtonListener(MouseButtonStateListener(MouseButtonStateEvent.wasPressedFilter) { e =>
		// Finds the slot that was pressed
		val positionInContainer = e.positionOverArea(container.bounds)
		container.components.find { _.bounds.contains(positionInContainer) }.flatMap { grid =>
			val positionInGrid = positionInContainer - grid.position
			grid.slotDisplays.find { _.bounds.contains(positionInGrid) }
		} match
		{
			case Some(clickedSlotVC) =>
				val slot = clickedSlotVC.content
				// On left mouse click, displays number selection screen (except for solved slots)
				if (e.isLeftMouseButton)
				{
					if (slot.nonSolved)
					{
						val isDeleting = isCtrlPressed
						SelectNumberPopUp.display(clickedSlotVC, slot.availableNumbers.toVector, isDeleteMode = isDeleting).foreach { num =>
							num.foreach { selectedNumber =>
								// Saves state before altering
								saveStates :+= content -> slot
								if (isDeleting)
									content = content.withSlotNumberNotAllowed(slot.position, selectedNumber)
								else
									content = content.withSlotNumber(slot.position, Some(selectedNumber))
							}
						}
					}
				}
				// Changes highlighting on right click
				else if (e.isRightMouseButton)
				{
					if (currentlySelectedSlot.exists { _.position == slot.position } || slot.isSolved)
					{
						currentlySelectedSlot = None
						currentHighlightedLinks = Vector()
					}
					else
					{
						val chains =
						{
							// On ctrl + click on twin slot, highlights twin chains
							if (isCtrlPressed && slot.availableNumbers.size == 2)
							{
								highlightIsTwinRestricted = true
								content.halfPairsGraph.twinChainsFrom(slot)
							}
							else
							{
								highlightIsTwinRestricted = false
								content.halfPairsGraph.chainsFrom(slot)
							}
						}
						
						currentHighlightedLinks = chains.flatMap { branch => branch.paired }.distinctBy { pair =>
							Set(pair._1, pair._2) }
						currentlySelectedSlot = Some(slot)
					}
				}
			case None =>
				currentlySelectedSlot = None
				currentHighlightedLinks = Vector()
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
		
		private val modifiedColor = colorScheme.secondary.forBackground(backgroundColor)
		private val relatedColor = colorScheme.primary.forBackground(backgroundColor)
		
		
		// IMPLEMENTED	------------------------
		
		override def drawLevel = Normal
		
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
		
		private val possibleColor = colorScheme.primary.forBackground(backgroundColor).withAlpha(0.55)
		private val takenColor = colorScheme.secondary.forBackground(backgroundColor).withAlpha(0.55)
		
		
		// IMPLEMENTED	-------------------------
		
		override def drawLevel = Normal
		
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
		val color = colorScheme.primary.forBackground(backgroundColor).withAlpha(0.88)
		val twinsColor = colorScheme.secondary.forBackground(backgroundColor).withAlpha(0.88)
		
		override def drawLevel = Normal
		
		override def draw(drawer: Drawer, bounds: Bounds) =
		{
			if (currentHighlightedLinks.nonEmpty)
			{
				// Draws a line from each chain link to the next step
				drawer.onlyEdges(if (highlightIsTwinRestricted) twinsColor else color).withStroke(4).disposeAfter { d =>
					currentHighlightedLinks.foreach { case (first, second) =>
						val origin = slotPositionToPixels(first.position, bounds)
						val target = slotPositionToPixels(second.position, bounds)
						d.draw(Line(origin, target))
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
