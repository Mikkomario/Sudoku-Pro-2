package sudoku.model

import utopia.reflection.component.context.TextContextLike
import utopia.reflection.component.drawing.immutable.BorderDrawer
import utopia.reflection.shape.Border

/**
 * Specifies slot border display visuals
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
case class BorderSettings(gridBorderWidth: Double, slotBorderWidth: Double)
{
	def slotBorder(implicit context: TextContextLike) = Border.square(slotBorderWidth, context.textColor.timesAlpha(0.66))
	def gridBorder(implicit context: TextContextLike) = Border.square(gridBorderWidth, context.textColor)
	
	def slotBorderDrawer(implicit context: TextContextLike) = new BorderDrawer(slotBorder)
	def gridBorderDrawer(implicit context: TextContextLike) = new BorderDrawer(gridBorder)
}
