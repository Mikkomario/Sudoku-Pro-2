package sudoku.view

import sudoku.controller.Solver
import sudoku.model.SudokuState
import utopia.genesis.event.MouseMoveEvent
import utopia.genesis.handling.MouseMoveListener
import utopia.inception.handling.immutable.Handleable
import utopia.reflection.component.Area
import utopia.reflection.component.context.ColorContext
import utopia.reflection.component.drawing.immutable.BorderDrawer
import utopia.reflection.component.swing.StackableAwtComponentWrapperWrapper
import utopia.reflection.component.swing.button.TextButton
import utopia.reflection.component.swing.label.TextLabel
import utopia.reflection.container.stack.StackLayout.Trailing
import utopia.reflection.container.swing.{AwtContainerRelated, Stack}
import utopia.reflection.shape.{Border, SymmetricStackSizeConstraint}
import utopia.reflection.shape.LengthExtensions._
import utopia.reflection.localization.LocalString._
import utopia.reflection.shape.Alignment.Center

/**
 * This view controller is used for handling the whole puzzle
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
class MainVC(initialSudoku: SudokuState) extends StackableAwtComponentWrapperWrapper with AwtContainerRelated
{
	import DefaultContext._
	
	// ATTRIBUTES	------------------------
	
	private implicit val languageCode: String = "en"
	private val bgColor = colorScheme.primary
	private implicit val context: ColorContext = baseContext.inContextWithBackground(bgColor)
	
	private val sudokuVC = new SudokuVC(initialSudoku, context)
	private val solveNextButton = context.forTextComponents(Center).forSecondaryColorButtons.use { implicit btnC =>
		TextButton.contextual("Next") {
			val result = Solver(currentSudoku)
			if (result.wasSuccess)
			{
				println(result.description.getOrElse("Success"))
				currentSudoku = result.newState
				sudokuVC.highlight(result.modifiedSlots, result.relatedSlots)
			}
			else
				println("Can't solve next :(")
		}
	}
	private val numberButtons = context.forTextComponents(Center).forPrimaryColorButtons.use { implicit btnC =>
		
		val borderDrawer = new BorderDrawer(Border.raised(2, btnC.buttonColor, 0.25))
		
		(1 to 9).map { number =>
			val label = TextLabel.contextualWithBackground(btnC.buttonColor, number.toString.noLanguageLocalizationSkipped)
			label.addCustomDrawer(borderDrawer)
			label.addConstraint(SymmetricStackSizeConstraint)
			label.addMouseMoveListener(new NumberHoverListener(label, number))
			label
		}
	}
	private val view = Stack.buildColumnWithContext(layout = Trailing) { s =>
		s += Stack.buildRowWithContext() { row => numberButtons.foreach { row += _ } }
		s += sudokuVC
		s += solveNextButton
	}.framed(margins.medium.any.square, bgColor)
	
	
	// COMPUTED	----------------------------
	
	private def currentSudoku = sudokuVC.content
	private def currentSudoku_=(newState: SudokuState) = sudokuVC.content = newState
	
	
	// IMPLEMENTED	------------------------
	
	override def component = view.component
	
	override protected def wrapped = view
	
	
	// NESTED	----------------------------
	
	private class NumberHoverListener(component: Area, number: Int) extends MouseMoveListener with Handleable
	{
		override def onMouseMove(event: MouseMoveEvent) =
		{
			val bounds = component.bounds
			if (event.enteredArea(bounds))
				sudokuVC.highlightNumber(number)
			else if (event.exitedArea(bounds))
				sudokuVC.endHighlightOfNumber(number)
		}
	}
}
