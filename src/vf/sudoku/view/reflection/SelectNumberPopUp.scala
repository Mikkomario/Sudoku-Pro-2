package vf.sudoku.view.reflection

import utopia.firmament.drawing.immutable.BackgroundDrawer
import utopia.flow.view.mutable.eventful.EventfulPointer
import utopia.genesis.handling.event.consume.ConsumeChoice.Consume
import utopia.genesis.handling.event.mouse.MouseButtonStateListener
import utopia.paradigm.color.Color
import utopia.paradigm.enumeration.Alignment.Center
import utopia.reflection.component.swing.label.ItemLabel
import utopia.reflection.component.swing.template.AwtComponentRelated
import utopia.reflection.component.template.ReflectionComponentLike
import utopia.reflection.container.swing.layout.multi.Stack
import utopia.reflection.container.swing.window.Popup
import utopia.reflection.container.swing.window.Popup.PopupAutoCloseLogic.WhenClickedOutside
import utopia.reflection.controller.data.ContainerSelectionManager
import vf.sudoku.view.reflection.vc.SlotVC

/**
 * This pop-up can be used for selecting a number
 * @author Mikko Hilpinen
 * @since 25.4.2020, v1
 */
object SelectNumberPopUp
{
	import DefaultContext._
	
	// OTHER	----------------------------
	
	/**
	 * Displays a number selection pop-up
	 * @param over Component over which the pop-up is shown
	 * @param numbers Possible numbers that can be selected from
	 * @param isDeleteMode Whether layout should be highlighted to deletion
	 * @return Future of the pop-up closing and number selection
	 */
	def display(over: ReflectionComponentLike with AwtComponentRelated, numbers: Vector[Int], isDeleteMode: Boolean = false) =
	{
		val background = {
			if (isDeleteMode)
				Color.red.withSaturation(0.25)
			else
				Color.green.withSaturation(0.25)
		}
		
		baseContext.against(background).use { implicit context =>
			// Creates the display components
			val selectedNumberPointer = EventfulPointer[Option[Int]](None)
			val numbersContainer = {
				if (numbers.size == 4 || numbers.size == 9)
					new GridContainer[ItemLabel[Int]]
				else
					Stack.row[ItemLabel[Int]]()
			}
			val manager = ContainerSelectionManager.forStatelessItems(numbersContainer,
				BackgroundDrawer(Color.blue.timesSaturation(0.22)), numbers.sorted) { i =>
				val label = SlotVC.makeNumberLabel(i)
				label.component.setFocusable(true)
				label.setHandCursor()
				label.addMouseButtonListener(MouseButtonStateListener.leftPressed.over(label.bounds) { _ =>
					selectedNumberPointer.value = Some(label.content)
					Consume("Number label selected")
				})
				label
			}
			
			if (isDeleteMode)
				numbersContainer.background = Color.red.withSaturation(0.25)
			else
				numbersContainer.background = Color.green.withSaturation(0.25)
			context.withCenteredText.use { implicit txtC =>
				numbersContainer.addCustomDrawer(borderSettings.gridBorderDrawer)
			}
			
			// Displays the components in a pop-up
			val popup = Popup(over, numbersContainer, actorHandler,
				resizeAlignment = Center, autoCloseLogic = WhenClickedOutside) {
				(cSize, wSize) => (cSize/2 - wSize/2).toPoint }
			
			// Displays pop-up
			popup.display()
			
			// Adds number selection listening
			selectedNumberPointer.addListener { e =>
				if (e.newValue.isDefined)
					popup.close()
				manager.value = e.newValue
			}
			
			popup.closeFuture.map { _ => selectedNumberPointer.value }
		}
	}
}
