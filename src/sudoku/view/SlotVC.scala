package sudoku.view

import sudoku.model.SolvableGroupType.{Column, Grid, Row}
import sudoku.model.{Slot, SolvableGroupType}
import utopia.firmament.component.display.RefreshableWithPointer
import utopia.firmament.context.ColorContext
import utopia.firmament.drawing.template.TextDrawerLike
import utopia.firmament.localization.DisplayFunction
import utopia.firmament.localization.LocalString._
import utopia.firmament.model.stack.LengthExtensions._
import utopia.firmament.model.stack.StackInsets
import utopia.genesis.graphics.DrawLevel.Normal
import utopia.genesis.graphics.MeasuredText
import utopia.genesis.text.Font
import utopia.paradigm.color.Color
import utopia.paradigm.enumeration.Alignment
import utopia.paradigm.enumeration.Alignment.{BottomLeft, Center, TopLeft, TopRight}
import utopia.reflection.component.swing.label.ItemLabel
import utopia.reflection.component.swing.template.StackableAwtComponentWrapperWrapper

object SlotVC
{
	import DefaultContext._
	
	/**
	 * Creates a slot-number label
	 * @param firstItem The first displayed item
	 * @param displayFunction Display function (default = toString)
	 * @param context Component creation (color) context
	 * @tparam A Type of displayed item
	 * @return A new label
	 */
	def makeNumberLabel[A](firstItem: A, displayFunction: DisplayFunction[A] = DisplayFunction.raw)
						  (implicit context: ColorContext) =
	{
		context.forTextComponents.withCenteredText.withTextInsets(StackInsets.symmetric(margins.medium.upscaling)).use { implicit txtC =>
			val label = ItemLabel.contextual(firstItem, displayFunction)
			label.addCustomDrawer(borderSettings.slotBorderDrawer)
			label
		}
	}
}

/**
 * A view component for a single sudoku slot
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
class SlotVC(initialSlot: Slot)(implicit parentContext: ColorContext)
	extends StackableAwtComponentWrapperWrapper with RefreshableWithPointer[Slot]
{
	import DefaultContext._
	
	// ATTRIBUTES	--------------------------
	
	private val label = SlotVC.makeNumberLabel(initialSlot, DisplayFunction.noLocalization[Slot] {
		_.number.map { _.toString }.getOrElse("").noLanguage })
	
	private val halfPlaceDrawers = SolvableGroupType.values.map { new HalfPlaceDrawer(_) }
	
	
	// INITIAL CODE	--------------------------
	
	if (initialSlot.nonSolved)
	{
		label.addCustomDrawer(AvailableNumbersDrawer)
		halfPlaceDrawers.foreach { label.addCustomDrawer(_) }
	}
	
	// Updates custom drawing (only available when not solved)
	contentPointer.addListener { e =>
		// println(s"Updating view for slot ${e.newValue}")
		if (e.newValue.isSolved != e.oldValue.isSolved)
		{
			if (e.newValue.isSolved)
			{
				label.removeCustomDrawer(AvailableNumbersDrawer)
				halfPlaceDrawers.foreach { label.removeCustomDrawer(_) }
				// println(s"${e.newValue} is now solved. Removes custom drawers.")
			}
			else
			{
				label.addCustomDrawer(AvailableNumbersDrawer)
				halfPlaceDrawers.foreach { label.addCustomDrawer(_) }
				// println(s"${e.newValue} is no longer solved. Adds custom drawers")
			}
		}
		label.repaint()
	}
	
	
	// IMPLEMENTED	--------------------------
	
	override protected def wrapped = label
	
	override def contentPointer = label.contentPointer
	
	
	// NESTED	------------------------------
	
	private object AvailableNumbersDrawer extends TextDrawerLike
	{
		override lazy val font: Font = parentContext.font * 0.5
		
		override def insets: StackInsets = StackInsets.any
		override def color: Color = Color.textBlack
		override def alignment: Alignment = Center
		
		override def drawLevel = Normal
		
		override def text = {
			val text = {
				if (content.isSolved)
					""
				else {
					val numbers = content.availableNumbers
					if (numbers.size >= 9)
						""
					else if (numbers.size > 3)
						"..."
					else
						numbers.toVector.sorted.mkString("")
				}
			}
			MeasuredText(text, fontMetricsWith(font), Center)
		}
	}
	
	private class HalfPlaceDrawer(targetGroupType: SolvableGroupType) extends TextDrawerLike
	{
		override lazy val font: Font = parentContext.font * 0.5
		override lazy val insets: StackInsets = StackInsets.symmetric(margins.small.any)
		override lazy val color: Color = targetGroupType match {
			case Grid => Color.green.darkenedBy(2)
			case Row => Color.blue
			case Column => Color.red
		}
		override lazy val alignment: Alignment = targetGroupType match {
			case Grid => TopLeft
			case Row => BottomLeft
			case Column => TopRight
		}
		
		override def drawLevel = Normal
		
		override def text = {
			val text = {
				if (content.isSolved)
					""
				else
					content.halfPlaceFor.get(targetGroupType) match {
						case Some(numbers) => numbers.toVector.sorted.mkString("")
						case None => ""
					}
			}
			MeasuredText(text, fontMetricsWith(font), alignment)
		}
	}
}
