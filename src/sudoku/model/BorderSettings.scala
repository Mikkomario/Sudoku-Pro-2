package sudoku.model

import utopia.genesis.color.Color
import utopia.reflection.component.drawing.immutable.BorderDrawer
import utopia.reflection.shape.Border

/**
 * Specifies slot border display visuals
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
case class BorderSettings(borderColor: Color, gridBorderWidth: Double, slotBorderWidth: Double)
{
	val slotBorder = Border.square(slotBorderWidth, borderColor.timesAlpha(0.66))
	val gridBorder = Border.square(gridBorderWidth, borderColor)
	
	val slotBorderDrawer = new BorderDrawer(slotBorder)
	val gridBorderDrawer = new BorderDrawer(gridBorder)
}
