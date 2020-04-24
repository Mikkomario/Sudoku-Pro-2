package sudoku.model

/**
 * Simply an x-y-coordinate pair on the grid
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
case class Position(x: Int, y: Int)
{
	override def toString = s"($x, $y)"
}
