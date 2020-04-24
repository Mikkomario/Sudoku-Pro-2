package sudoku.controller

import sudoku.model.{Position, SolveResult, SudokuState}
import utopia.flow.util.CollectionExtensions._
import utopia.genesis.shape.Axis.{X, Y}
import utopia.genesis.shape.Axis2D

object XWing
{
	/**
	 * Variants of this algorithm that each target different axes
	 */
	val variants = Axis2D.values.map { new XWing(_) }
}

/**
 * Finds an X-wing from the sudoku, based on half-number pairs
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 */
class XWing(axis: Axis2D) extends SolveAlgorithm
{
	private implicit val languageCode: String = "en"
	
	override def name = "Finds x-wings"
	
	override def apply(sudoku: SudokuState) =
	{
		val lines = sudoku.slotLinesAlong(axis)
		// Collects half-number pairs along the lines
		// number + aligned (slot) index 1 + aligned (slot) index 2 => [Perpendicular (line) index]
		var pairs = Map[(Int, Int, Int), Vector[Int]]()
		lines.foreachWithIndex { (line, perpendicularIndex) =>
			// Number -> Number's indices in this line
			val alignedIndicesPerNumber = line.slots.mapWithIndex { (slot, alignedIndex) =>
				slot.halfPlacesFor(axis).map { number => number -> alignedIndex }
			}.flatten.toVector.asMultiMap
			
			// Adds the found pairs to the map
			alignedIndicesPerNumber.foreach { case (number, alignedIndices) =>
				if (alignedIndices.size > 1)
				{
					val key = (number, alignedIndices.head, alignedIndices(1))
					if (pairs.contains(key))
						pairs += key -> (pairs(key) :+ perpendicularIndex)
					else
						pairs += key -> Vector(perpendicularIndex)
				}
			}
		}
		
		// Checks if there were any two lines where same number occupied the same slots
		// If there were, finds out all the slots between those lines at those specified slot indices and makes sure
		// they won't contain that number
		val matchingPairs = pairs.filter { _._2.size > 1 }
		val illegalNumbersPerPosition = matchingPairs.toVector.flatMap { case (key, perpendicularIndices) =>
			val (number, alignIndex1, alignIndex2) = key
			val firstLineIndex = perpendicularIndices.min
			val lastLineIndex = perpendicularIndices.max
			val affectedLinesRange = (firstLineIndex + 1) until lastLineIndex
			affectedLinesRange.flatMap { perpendicularIndex =>
				val illegalPositions = Vector(alignIndex1, alignIndex2).map { alignIndex =>
					axis match
					{
						case X => Position(alignIndex, perpendicularIndex)
						case Y => Position(perpendicularIndex, alignIndex)
					}
				}
				illegalPositions.map { _ -> number }
			}
		}.asMultiMap
		
		// Finally applies changes to the sudoku
		if (illegalNumbersPerPosition.isEmpty)
			SolveResult.failure(sudoku)
		else
		{
			sudoku.trySolveSlots { slot =>
				illegalNumbersPerPosition.get(slot.position).map { illegalNumbers =>
					slot.withNotAllowed(illegalNumbers.toSet)
				}
			} match
			{
				case Some(result) =>
					val (newState, modifiedSlots) = result
					// Finds the x-wing corners as related slots
					val affectingSlots = matchingPairs.flatMap { case (key, lineIndices) =>
						Vector(key._2, key._3).flatMap { slotIndex =>
							lineIndices.map { lineIndex =>
								lines(lineIndex)(slotIndex)
							}
						}
					}.toSet
					SolveResult.success(newState, modifiedSlots.toSet, affectingSlots, Some(name))
					
				case None => SolveResult.failure(sudoku)
			}
		}
	}
}
