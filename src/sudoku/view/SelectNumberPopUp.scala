package sudoku.view

import sudoku.model.BorderSettings
import utopia.flow.datastructure.mutable.PointerWithEvents
import utopia.genesis.color.Color
import utopia.genesis.event.ConsumeEvent
import utopia.genesis.handling.MouseButtonStateListener
import utopia.reflection.component.ComponentLike
import utopia.reflection.component.drawing.immutable.BackgroundDrawer
import utopia.reflection.component.swing.AwtComponentRelated
import utopia.reflection.component.swing.label.ItemLabel
import utopia.reflection.container.swing.Stack
import utopia.reflection.container.swing.window.Popup
import utopia.reflection.controller.data.ContainerSelectionManager
import utopia.reflection.shape.Alignment.Center
import utopia.reflection.shape.Margins
import utopia.reflection.util.ComponentContextBuilder

import scala.concurrent.ExecutionContext

/**
 * This pop-up can be used for selecting a number
 * @author Mikko Hilpinen
 * @since 25.4.2020, v1
 */
object SelectNumberPopUp
{
	// OTHER	----------------------------
	
	/**
	 * Displays a number selection pop-up
	 * @param over Component over which the pop-up is shown
	 * @param possibleNumbers Possible numbers that can be selected from
	 * @param baseCB A component context builder (implicit)
	 * @param margins Margin settings (implicit)
	 * @param borderSettings Border settings (implicit)
	 * @param exc Execution context (implicit)
	 * @return Future of the pop-up closing and number selection
	 */
	def display(over: ComponentLike with AwtComponentRelated, possibleNumbers: Vector[Int])
			   (implicit baseCB: ComponentContextBuilder, margins: Margins, borderSettings: BorderSettings,
				exc: ExecutionContext) =
	{
		// Creates the display components
		val selectedNumberPointer = new PointerWithEvents[Option[Int]](None)
		val numbersContainer =
		{
			if (possibleNumbers.size == 4 || possibleNumbers.size == 9)
				new GridContainer[ItemLabel[Int]]
			else
				Stack.row[ItemLabel[Int]]()
		}
		val manager = ContainerSelectionManager.forStatelessItems(numbersContainer,
			new BackgroundDrawer(Color.blue.timesSaturation(0.22)), possibleNumbers.sorted) { i =>
			val label = SlotVC.makeNumberLabel(i)
			label.component.setFocusable(true)
			label.setHandCursor()
			label.addMouseButtonListener(MouseButtonStateListener.onLeftPressedInside(label.bounds) { e =>
				selectedNumberPointer.value = Some(label.content)
				Some(ConsumeEvent("Number label selected"))
			})
			label
		}
		
		numbersContainer.background = Color.gray(0.66)
		numbersContainer.addCustomDrawer(borderSettings.gridBorderDrawer)
		
		// Displays the components in a pop-up
		val popup = Popup(over, numbersContainer, baseCB.actorHandler,
			resizeAlignment = Center) { (cSize, wSize) => (cSize/2 - wSize/2).toPoint }
		
		popup.display()
		
		// Adds number selection listening
		selectedNumberPointer.addListener { e =>
			if (e.newValue.isDefined)
				popup.close()
			manager.value = e.newValue
		}
		
		// Displays pop-up
		popup.closeFuture.map { _ => selectedNumberPointer.value }
	}
}
