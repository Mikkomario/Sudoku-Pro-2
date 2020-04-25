package sudoku.controller

import sudoku.model.{SolveResult, SudokuState}

/**
 * This algorithm finds closed half-number chains from the sudoku and limits the possible numbers in chain participants.
 * A closed chain means a group of 3 or more slots where the numbers go like: 12 -> 23 -> 31 and each is connected via
 * a half-number link (slots are the only possible places for those numbers in a group)
 * @author Mikko Hilpinen
 * @since 25.4.2020, v1
 */
object ClosedChainsRule extends SolveAlgorithm
{
	// ATTRIBUTES	----------------------
	
	private implicit val languageCode: String = "en"
	
	
	// IMPLEMENTED	----------------------
	
	override def name = "Limits possible numbers within closed chains"
	
	override def apply(sudoku: SudokuState) =
	{
		// Finds the closed chains within the sudoku ([slots -> numbers])
		val chains = sudoku.halfPairsGraph.closedChains
		if (chains.nonEmpty)
		{
			println("Found chains:")
			chains.foreach { case (slots, numbers) =>
				val numbersString = numbers.toVector.sorted.mkString("")
				println(s"$numbersString: ${ slots.map { _.position }.mkString(", ") }")
			}
		}
		// Finds the number restrictions on slots, filters to those that would have effect
		val changes = chains.flatMap { case (slots, numbers) =>
			slots.filterNot { slot => slot.mustBe.forall(numbers.contains) }.map { slot => slot -> numbers }
		}
		
		if (changes.nonEmpty)
		{
			println("Changes to apply: ")
			changes.foreach { case (slot, numbers) => s"- ${slot.position} is restricted to ${numbers.toVector.sorted.mkString("")}" }
			
			// Applies changes to sudoku
			sudoku.trySolveSlots { slot =>
				val changesForSlot = changes.filter { _._1 == slot }.map { _._2 }
				if (changesForSlot.nonEmpty)
					Some(changesForSlot.foldLeft(slot) { (s, numbers) => s.withMustBe(numbers) })
				else
					None
			} match
			{
				case Some(result) =>
					val (newState, modifiedSlots) = result
					// Finds the slot chains that caused the changes in modified slots
					val affectingSlots = chains.filter { _._1.exists { slot => modifiedSlots.exists {
						_.position == slot.position } } }.flatMap { _._1 }
					SolveResult.success(newState, modifiedSlots.toSet, affectingSlots, Some(name))
				case None => SolveResult.failure(sudoku)
			}
		}
		else
			SolveResult.failure(sudoku)
	}
}
