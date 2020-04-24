package sudoku.model

import sudoku.model.SolvableGroupType.{Column, Row}
import utopia.genesis.shape.Axis.{X, Y}
import utopia.genesis.shape.Axis2D

/**
 * Slot has a specific position on the sudoku and it may contain a number
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 * @param position The x-y -coordinate of this slot
 * @param number The number in this slot (None if empty)
 * @param cantBe Numbers this slot is not allowed to have (negative limit)
 * @param mustBe Numbers this slot is limited to (positive limit)
 * @param halfPlaceFor Numbers for which this slot is one of the two places to go in a local system (markers).
 *                     Separated for each local system group type.
 */
case class Slot(position: Position, number: Option[Int] = None, cantBe: Set[Int] = Set(), mustBe: Set[Int] = Set(),
				halfPlaceFor: Map[SolvableGroupType, Set[Int]] = Map()) extends Solvable
{
	// ATTRIBUTES	--------------------
	
	/**
	 * Numbers that can be fit into this slot, according to current data
	 */
	lazy val availableNumbers =
	{
		number match
		{
			case Some(existing) => Set(existing)
			case None =>
				val pool = if (mustBe.isEmpty) (1 to 9).toSet else mustBe
				pool -- cantBe
		}
	}
	
	
	// COMPUTED	------------------------
	
	/**
	 * @return Whether this slot has already been solved
	 */
	override def isSolved = number.isDefined
	
	/**
	 * @return Two number pairs for numbers that can only be placed in this or another slot
	 */
	def halfPairs = halfPlaceFor.values.filter { _.size == 2 }.toSet
	
	/**
	 * @return An ascii representation of this slot (without borders)
	 */
	def ascii = number.map { _.toString }.getOrElse(" ")
	
	
	// IMPLEMENTED	--------------------
	
	override def toString =
	{
		number match
		{
			case Some(result) => s"$position: $result"
			case None =>
				val numbers = availableNumbers.toVector.sorted
				val numbersDescription =
				{
					if (availableNumbers.size >= 9)
						"?"
					else if (availableNumbers.size == 2)
						s"${numbers.head}|${numbers(1)}"
					else
						s"[${numbers.mkString("")}]"
				}
				s"$position: $numbersDescription"
		}
	}
	
	
	// OTHER	------------------------
	
	/**
	 * @param axis Targeted axis
	 * @return Numbers that must be placed on this slot or exactly one other slot on specified axis
	 */
	def halfPlacesFor(axis: Axis2D) =
	{
		val group = axis match
		{
			case X => Row
			case Y => Column
		}
		halfPlaceFor.getOrElse(group, Set())
	}
	
	/**
	 * @param numbers Numbers that are not allowed in this slot
	 * @return A copy of this slot with specified numbers registered
	 */
	def withNotAllowed(numbers: Set[Int]) =
	{
		// Adds both "can't be" numbers and removes matching "half" numbers
		val newHalfPlace = halfPlaceFor.map { case (group, halfNumbers) => group -> (halfNumbers -- numbers) }
		val newCantBe = cantBe ++ numbers
		
		if (newCantBe == cantBe && newHalfPlace == halfPlaceFor)
			this
		else
			copy(cantBe = newCantBe, halfPlaceFor = newHalfPlace)
	}
	
	/**
	 * @param numbers A pool of numbers, of which this slot must fill one
	 * @return A copy of this slot with specified numbers registered as musts
	 */
	def withMustBe(numbers: Set[Int]) =
	{
		val newMustBe = if (mustBe.isEmpty) numbers else mustBe & numbers
		if (mustBe == newMustBe)
			this
		else
			copy(mustBe = newMustBe)
	}
	
	/**
	 * @param groupType Type of group for which the half places apply
	 * @param numbers Numbers that must be either in this slot or in 1 other slot in the group
	 * @return A copy of this slot with numbers recorded
	 */
	def withHalfPlaces(groupType: SolvableGroupType, numbers: Set[Int]) =
	{
		val existingHalfPlaces = halfPlaceFor.getOrElse(groupType, Set())
		val newHalfPlaces = existingHalfPlaces ++ numbers
		if (newHalfPlaces != existingHalfPlaces)
			copy(halfPlaceFor = halfPlaceFor + (groupType -> newHalfPlaces))
		else
			this
	}
}
