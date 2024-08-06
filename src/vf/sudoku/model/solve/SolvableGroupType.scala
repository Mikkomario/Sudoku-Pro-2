package vf.sudoku.model.solve

import utopia.firmament.localization.LocalString

/**
 * An enumeration for different types of solvable groups
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
sealed trait SolvableGroupType
{
	/**
	 * @return Name of this group
	 */
	def name: LocalString
}

object SolvableGroupType
{
	private implicit val languageCode: String = "en"
	
	/**
	 * Grid as a type of slot collection
	 */
	case object Grid extends SolvableGroupType
	{
		override def name = "grid"
	}
	
	/**
	 * Row as a type of slot collection
	 */
	case object Row extends SolvableGroupType
	{
		override def name = "row"
	}
	
	/**
	 * Column as a type of slot collection
	 */
	case object Column extends SolvableGroupType
	{
		override def name = "column"
	}
	
	/**
	 * All possible group types
	 */
	val values = Vector(Grid, Row, Column)
}
