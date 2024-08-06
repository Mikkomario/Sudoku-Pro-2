package sudoku.model

import utopia.firmament.context.TextContext
import utopia.firmament.drawing.immutable.BorderDrawer
import utopia.firmament.model.Border

/**
 * Specifies slot border display visuals
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
case class BorderSettings(gridBorderWidth: Double, slotBorderWidth: Double)
{
	def slotBorder(implicit context: TextContext) = Border.symmetric(slotBorderWidth, context.textColor.timesAlpha(0.66))
	def gridBorder(implicit context: TextContext) = Border.symmetric(gridBorderWidth, context.textColor)
	
	def slotBorderDrawer(implicit context: TextContext) = BorderDrawer(slotBorder)
	def gridBorderDrawer(implicit context: TextContext) = BorderDrawer(gridBorder)
}
