package vf.sudoku.model.grid

import utopia.flow.operator.equality.EqualsBy
import vf.sudoku.model.solve.Solvable

import scala.language.implicitConversions

object Line
{
	// Lines can be treated like vectors
	implicit def lineToVector[A <: Solvable](line: Line[A]): Vector[A] = line.items
	
	/**
	 * @param items Items that form the line
	 * @tparam A Type of items
	 * @return A line based on the items
	 */
	def apply[A <: Solvable](items: Vector[A]) = new Line(items)
}

/**
 * A line of items
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
class Line[+A <: Solvable](val items: Vector[A]) extends Solvable with EqualsBy
{
	// IMPLEMENTED	--------------------
	
	override protected def equalsProperties: Seq[Any] = items
	
	override def isSolved = items.forall { _.isSolved }
}
