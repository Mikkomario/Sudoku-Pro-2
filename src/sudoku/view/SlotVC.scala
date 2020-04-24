package sudoku.view

import sudoku.model.SolvableGroupType.{Column, Grid, Row}
import sudoku.model.{BorderSettings, Slot, SolvableGroupType}
import utopia.genesis.color.Color
import utopia.reflection.component.RefreshableWithPointer
import utopia.reflection.component.drawing.immutable.TextDrawContext
import utopia.reflection.component.drawing.template.DrawLevel.Normal
import utopia.reflection.component.drawing.template.TextDrawer
import utopia.reflection.component.swing.StackableAwtComponentWrapperWrapper
import utopia.reflection.component.swing.label.ItemLabel
import utopia.reflection.localization.{DisplayFunction, LocalizedString}
import utopia.reflection.shape.{Margins, StackInsets}
import utopia.reflection.util.{ComponentContext, ComponentContextBuilder}
import utopia.reflection.shape.LengthExtensions._
import utopia.reflection.localization.LocalString._
import utopia.reflection.shape.Alignment.{BottomLeft, Center, TopLeft, TopRight}

/**
 * A view component for a single sudoku slot
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
class SlotVC(initialSlot: Slot)(implicit baseCB: ComponentContextBuilder, margins: Margins, borderSettings: BorderSettings)
	extends StackableAwtComponentWrapperWrapper with RefreshableWithPointer[Slot]
{
	// ATTRIBUTES	--------------------------
	
	private implicit val context: ComponentContext = baseCB.withInsets(StackInsets.symmetric(margins.medium.upscaling)).result
	
	private val label = ItemLabel.contextual(initialSlot, DisplayFunction.noLocalization[Slot] {
		_.number.map { _.toString }.getOrElse("").noLanguage })
	
	private val halfPlaceDrawers = SolvableGroupType.values.map { new HalfPlaceDrawer(_) }
	
	
	// INITIAL CODE	--------------------------
	
	label.addCustomDrawer(borderSettings.slotBorderDrawer)
	if (initialSlot.nonSolved)
	{
		label.addCustomDrawer(AvailableNumbersDrawer)
		halfPlaceDrawers.foreach { label.addCustomDrawer(_) }
	}
	
	// Updates custom drawing (only available when not solved)
	contentPointer.addListener { e =>
		if (e.newValue.isSolved != e.oldValue.isSolved)
		{
			if (e.newValue.isSolved)
			{
				label.addCustomDrawer(AvailableNumbersDrawer)
				halfPlaceDrawers.foreach { label.addCustomDrawer(_) }
			}
			else
			{
				label.removeCustomDrawer(AvailableNumbersDrawer)
				halfPlaceDrawers.foreach { label.removeCustomDrawer(_) }
			}
		}
		label.repaint()
	}
	
	
	// IMPLEMENTED	--------------------------
	
	override protected def wrapped = label
	
	override def contentPointer = label.contentPointer
	
	
	// NESTED	------------------------------
	
	object AvailableNumbersDrawer extends TextDrawer
	{
		override val drawContext = TextDrawContext(context.font * 0.5, alignment = Center)
		
		override def text =
		{
			if (content.isSolved)
				LocalizedString.empty
			else
			{
				val numbers = content.availableNumbers
				if (numbers.size >= 9)
					LocalizedString.empty
				else if (numbers.size > 3)
					"...".noLanguageLocalizationSkipped
				else
					numbers.toVector.sorted.mkString("").noLanguageLocalizationSkipped
			}
		}
		
		override def drawLevel = Normal
	}
	
	class HalfPlaceDrawer(targetGroupType: SolvableGroupType) extends TextDrawer
	{
		override val drawContext =
		{
			val font = context.font * 0.5
			val insets = StackInsets.symmetric(margins.small.any)
			targetGroupType match
			{
				case Grid => TextDrawContext(font, Color.green.darkened(2), TopLeft, insets)
				case Row => TextDrawContext(font, Color.blue, BottomLeft, insets)
				case Column => TextDrawContext(font, Color.red, TopRight, insets)
			}
		}
		
		override def text =
		{
			if (content.isSolved)
				LocalizedString.empty
			else
			{
				content.halfPlaceFor.get(targetGroupType) match
				{
					case Some(numbers) => numbers.toVector.sorted.mkString("").noLanguageLocalizationSkipped
					case None => LocalizedString.empty
				}
			}
		}
		
		override def drawLevel = Normal
	}
}
