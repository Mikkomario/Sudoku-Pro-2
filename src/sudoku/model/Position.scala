package sudoku.model

import utopia.genesis.shape.Axis.{X, Y}
import utopia.genesis.shape.Axis2D

/**
 * Simply an x-y-coordinate pair on the grid
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
case class Position(x: Int, y: Int)
{
	override def toString = s"(${x + 1}, ${y + 1})"
	
	def along(axis: Axis2D) = axis match
	{
		case X => x
		case Y => y
	}
}
