package sudoku.view

import sudoku.model.{Position, Slot, SudokuState}
import utopia.firmament.component.display.RefreshableWithPointer
import utopia.firmament.context.ColorContext
import utopia.firmament.controller.data.ContainerContentDisplayer
import utopia.firmament.drawing.mutable.MutableCustomDrawableWrapper
import utopia.firmament.drawing.template.CustomDrawer
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.time.TimeExtensions._
import utopia.flow.view.mutable.eventful.{EventfulPointer, ResettableFlag}
import utopia.flow.view.template.eventful.FlagLike
import utopia.genesis.graphics.DrawLevel.Normal
import utopia.genesis.graphics.{DrawSettings, Drawer, StrokeSettings}
import utopia.genesis.handling.action.Actor
import utopia.genesis.handling.event.keyboard.Key.{CharKey, Control}
import utopia.genesis.handling.event.keyboard.{KeyStateListener, KeyboardEvents}
import utopia.genesis.handling.event.mouse.MouseButtonStateListener
import utopia.paradigm.color.Color
import utopia.paradigm.shape.shape2d.area.polygon.c4.bounds.Bounds
import utopia.paradigm.shape.shape2d.line.Line
import utopia.paradigm.shape.shape2d.vector.Vector2D
import utopia.reflection.component.swing.template.StackableAwtComponentWrapperWrapper

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
	extends StackableAwtComponentWrapperWrapper with RefreshableWithPointer[SudokuState]
		with MutableCustomDrawableWrapper
{
	import DefaultContext._
	import SudokuVC._
	
	// ATTRIBUTES	----------------------------
	
	private val backgroundColor = colorScheme.gray.light
	
	val contentPointer = EventfulPointer[SudokuState](initialState)
	private val gridsPointer = contentPointer.map { _.grids }
	
	private val highlightedNumberPointer = EventfulPointer.empty[Int]()
	private val lastHighlightedNumberPointer =
		highlightedNumberPointer.incrementalMap { _.getOrElse(1) } { (prev, event) => event.oldValue.getOrElse(prev) }
	
	private val container = new GridContainer[GridVC]
	
	private var _lastModifiedSlots = Set[Slot]()
	private var _relatedSlots = Set[Slot]()
	private val remainingHighlightLevelPointer = EventfulPointer(0.0)
	private val highlightedFlag = remainingHighlightLevelPointer.map { _ > 0 }
	
	private val numberHighlightLevelPointer = EventfulPointer(0.0)
	
	private var currentlySelectedSlot: Option[Slot] = None
	private var currentHighlightedLinks = Vector[Pair[Slot]]()
	private val twinRestrictedHighlightingFlag = ResettableFlag()
	
	private var isCtrlPressed = false
	private var saveStates = Vector[(SudokuState, Slot)]()
	
	
	// COMPUTED	-------------------------------
	
	private def gridDisplays = container.components
	private def slotDisplaysView = gridDisplays.view.flatMap { _.slotDisplays }
	
	private def highlightedNumber: Option[Int] = highlightedNumberPointer.value
	private def highlightedNumber_=(number: Int) = highlightedNumberPointer.value = Some(number)
	
	private def lastHighlightedNumber = lastHighlightedNumberPointer.value
	
	private def remainingHighlightLevel = remainingHighlightLevelPointer.value
	private def remainingHighlightLevel_=(newLevel: Double) = remainingHighlightLevelPointer.value = newLevel
	
	private def numberHighlightLevel = numberHighlightLevelPointer.value
	
	private def isHighlighted = highlightedFlag.value
	
	
	// INITIAL CODE	---------------------------
	
	container.background = backgroundColor
	
	parentContext.against(backgroundColor).use { implicit context =>
		ContainerContentDisplayer.forImmutableStates(container, gridsPointer) {
			_.position == _.position } { grid => new GridVC(grid) }
	}
	
	addCustomDrawer(ModifiedSlotsHighlighter)
	addCustomDrawer(NumberHighlighter)
	addCustomDrawer(LinksDrawer)
	parentContext.actorHandler += HighLightUpdater
	parentContext.actorHandler += NumberHighLightUpdater
	
	KeyboardEvents += KeyStateListener(Control) { e => isCtrlPressed = e.pressed }
	KeyboardEvents += KeyStateListener(CharKey('Z')).pressed { e =>
		// TODO: Use handle condition instead (requires refactoring these to pointers)
		if (e.keyboardState(Control) && saveStates.nonEmpty) {
			val (oldState, cancelSlot) = saveStates.last
			println(s"Cancelling last change: ${cancelSlot.position}")
			content = oldState
			highlight(Set(cancelSlot), Set())
			// FIXME: Doesn't draw cancelled slot data correctly!
			saveStates = saveStates.dropRight(1)
		}
	}
	
	addMouseButtonListener(MouseButtonStateListener.pressed { e =>
		// Finds the slot that was pressed
		val positionInContainer = e.position - container.position
		container.components.find { _.bounds.contains(positionInContainer) }.flatMap { grid =>
			val positionInGrid = positionInContainer - grid.position
			grid.slotDisplays.find { _.bounds.contains(positionInGrid) }
		} match {
			case Some(clickedSlotVC) =>
				val slot = clickedSlotVC.content
				// On left mouse click, displays number selection screen (except for solved slots)
				if (e.concernsLeft) {
					if (slot.nonSolved) {
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
				else if (e.concernsRight) {
					if (currentlySelectedSlot.exists { _.position == slot.position } || slot.isSolved) {
						currentlySelectedSlot = None
						currentHighlightedLinks = Vector()
					}
					else {
						val chains = {
							// On ctrl + click on twin slot, highlights twin chains
							if (isCtrlPressed && slot.availableNumbers.size == 2) {
								twinRestrictedHighlightingFlag.set()
								content.halfPairsGraph.twinChainsFrom(slot)
							}
							else {
								twinRestrictedHighlightingFlag.reset()
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
	})
	
	
	// IMPLEMENTED	----------------------------
	
	override def drawable = container
	
	override protected def wrapped = container
	
	
	// OTHER	--------------------------------
	
	/**
	 * Starts highlighting specified number
	 * @param number Number to be highlighted
	 */
	def highlightNumber(number: Int) = highlightedNumber = number
	
	/**
	 * Ends highlighting the specified number
	 * @param number Number to be no longer highlighted
	 */
	def endHighlightOfNumber(number: Int) = highlightedNumberPointer.update { _.filterNot { _ == number } }
	
	/**
	 * Highlights the specified slots in this view
	 * @param modifiedSlots Slots that were recently modified
	 * @param relatedSlots Slots that related to modifying
	 */
	def highlight(modifiedSlots: Set[Slot], relatedSlots: Set[Slot]) = {
		_lastModifiedSlots = modifiedSlots
		_relatedSlots = relatedSlots
		remainingHighlightLevel = 1.0
	}
	
	
	// NESTED	--------------------------------
	
	private object HighLightUpdater extends Actor
	{
		override def handleCondition: FlagLike = highlightedFlag
		
		override def act(duration: FiniteDuration) = {
			val highlightMod = duration / highlightDuration
			remainingHighlightLevelPointer.update { _ - highlightMod }
			repaint()
		}
	}
	
	private object NumberHighLightUpdater extends Actor
	{
		override val handleCondition: FlagLike =
			highlightedNumberPointer.mergeWith(numberHighlightLevelPointer) { (highlighted, level) =>
				if (highlighted.isDefined) level < 1 else level > 0
			}
		
		override def act(duration: FiniteDuration) = {
			val adjustment = duration / numberHighlightChangeDuration
			if (highlightedNumber.isDefined)
				numberHighlightLevelPointer.update { l => (l + adjustment) min 1 }
			else
				numberHighlightLevelPointer.update { l => (l - adjustment) max 0 }
			
			repaint()
		}
	}
	
	private object ModifiedSlotsHighlighter extends CustomDrawer
	{
		// ATTRIBUTES	------------------------
		
		private val modifiedColor = colorScheme.secondary.against(backgroundColor)
		private val relatedColor = colorScheme.primary.against(backgroundColor)
		
		
		// IMPLEMENTED	------------------------
		
		override def drawLevel = Normal
		
		override def opaque: Boolean = false
		
		override def draw(drawer: Drawer, bounds: Bounds) = {
			if (isHighlighted) {
				// Goes through each modified slot and draws a background for it
				highlightSlots(drawer, _lastModifiedSlots, modifiedColor)
				highlightSlots(drawer, _relatedSlots, relatedColor)
			}
		}
		
		
		// OTHER	----------------------------
		
		private def highlightSlots(drawer: Drawer, slots: Set[Slot], color: Color) = {
			if (slots.nonEmpty) {
				implicit val ds: DrawSettings = DrawSettings.onlyFill(color.timesAlpha(remainingHighlightLevel))
				gridDisplays.foreach { gridVC =>
					gridVC.slotDisplays.filter { slotVC => slots.contains(slotVC.content) }.foreach { slotVC =>
						val bounds = slotVC.bounds + gridVC.position
						drawer.draw(bounds)
					}
				}
			}
		}
	}
	
	private object NumberHighlighter extends CustomDrawer
	{
		// ATTRIBUTES	-------------------------
		
		private val possibleColor = colorScheme.primary.against(backgroundColor).withAlpha(0.55)
		private val takenColor = colorScheme.secondary.against(backgroundColor).withAlpha(0.55)
		
		
		// IMPLEMENTED	-------------------------
		
		override def drawLevel = Normal
		override def opaque: Boolean = false
		
		override def draw(drawer: Drawer, bounds: Bounds) = {
			if (numberHighlightLevel > 0) {
				val number = highlightedNumber.getOrElse(lastHighlightedNumber)
				
				// Draws the possible slots, then the taken slots
				highlightSlots(drawer, possibleColor) { slot => slot.nonSolved && slot.availableNumbers.contains(number) }
				highlightSlots(drawer, takenColor) { _.number.contains(number) }
			}
		}
		
		
		// OTHER	-----------------------------
		
		private def highlightSlots(drawer: Drawer, color: Color)(filter: Slot => Boolean) = {
			implicit val ds: DrawSettings = DrawSettings.onlyFill(color.timesAlpha(numberHighlightLevel))
			gridDisplays.foreach { gridVC =>
				gridVC.slotDisplays.filter { slotVC => filter(slotVC.content) }.foreach { slotVC =>
					val bounds = slotVC.bounds + gridVC.position
					drawer.draw(bounds)
				}
			}
		}
	}
	
	private object LinksDrawer extends CustomDrawer
	{
		private val color = colorScheme.primary.against(backgroundColor).withAlpha(0.88)
		private val twinsColor = colorScheme.secondary.against(backgroundColor).withAlpha(0.88)
		
		private val colorPointer = twinRestrictedHighlightingFlag.map { if (_) twinsColor else color }
		private val dsPointer = colorPointer.map { StrokeSettings(_, 4).toDrawSettings }
		
		override def drawLevel = Normal
		override def opaque: Boolean = false
		
		override def draw(drawer: Drawer, bounds: Bounds) = {
			if (currentHighlightedLinks.nonEmpty) {
				// Draws a line from each chain link to the next step
				implicit val ds: DrawSettings = dsPointer.value
				currentHighlightedLinks.foreach { link =>
					val origin = slotPositionToPixels(link.first.position, bounds)
					val target = slotPositionToPixels(link.second.position, bounds)
					drawer.draw(Line(origin, target))
				}
			}
		}
		
		private def slotPositionToPixels(position: Position, bounds: Bounds) =
			bounds.position + (Vector2D(position.x, position.y) / 9 * bounds.size) + bounds.size / 18
	}
}
