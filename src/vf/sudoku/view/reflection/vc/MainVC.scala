package vf.sudoku.view.reflection.vc

import vf.sudoku.view.reflection.DefaultContext._
import utopia.firmament.context.ColorContext
import utopia.firmament.drawing.immutable.BorderDrawer
import utopia.firmament.localization.LocalString._
import utopia.firmament.model.Border
import utopia.firmament.model.enumeration.StackLayout.Trailing
import utopia.firmament.model.stack.LengthExtensions._
import utopia.firmament.model.stack.modifier.SymmetricSizeModifier
import utopia.flow.operator.filter.{AcceptAll, Filter}
import utopia.flow.view.immutable.eventful.AlwaysTrue
import utopia.flow.view.template.eventful.FlagLike
import utopia.genesis.handling.event.mouse.{MouseMoveEvent, MouseMoveListener}
import utopia.paradigm.color.ColorRole.{Primary, Secondary}
import utopia.paradigm.shape.shape2d.area.polygon.c4.bounds.HasBounds
import utopia.reflection.component.swing.button.TextButton
import utopia.reflection.component.swing.label.TextLabel
import utopia.reflection.component.swing.template.StackableAwtComponentWrapperWrapper
import utopia.reflection.container.swing.AwtContainerRelated
import utopia.reflection.container.swing.layout.multi.Stack
import vf.sudoku.controller.solve.Solver
import vf.sudoku.model.solve.SudokuState

/**
 * This view controller is used for handling the whole puzzle
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
class MainVC(initialSudoku: SudokuState) extends StackableAwtComponentWrapperWrapper with AwtContainerRelated
{
	// ATTRIBUTES	------------------------
	
	private implicit val languageCode: String = "en"
	private val bgColor = colorScheme.primary
	private implicit val context: ColorContext = baseContext.against(bgColor)
	
	private val sudokuVC = new SudokuVC(initialSudoku, context)
	private val solveNextButton = (context.forTextComponents.withCenteredText/Secondary).use { implicit btnC =>
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
	private val numberButtons = (context.forTextComponents.withCenteredText/Primary).use { implicit btnC =>
		
		val borderDrawer = BorderDrawer(Border.raised(2, btnC.background, 0.25))
		
		(1 to 9).map { number =>
			val label = TextLabel.contextualWithBackground(btnC.background, number.toString.noLanguageLocalizationSkipped)
			label.addCustomDrawer(borderDrawer)
			label.addConstraint(SymmetricSizeModifier)
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
	
	private class NumberHoverListener(component: HasBounds, number: Int) extends MouseMoveListener
	{
		override def mouseMoveEventFilter: Filter[MouseMoveEvent] = AcceptAll
		override def handleCondition: FlagLike = AlwaysTrue
		
		override def onMouseMove(event: MouseMoveEvent) = {
			val bounds = component.bounds
			if (event.entered(bounds))
				sudokuVC.highlightNumber(number)
			else if (event.exited(bounds))
				sudokuVC.endHighlightOfNumber(number)
		}
	}
}
