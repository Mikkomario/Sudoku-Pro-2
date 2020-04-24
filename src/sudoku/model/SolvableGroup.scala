package sudoku.model

import utopia.flow.util.CollectionExtensions._

import scala.collection.immutable.VectorBuilder

/**
 * A common trait for groups of solvable items
 * @author Mikko Hilpinen
 * @since 24.4.2020, v1
 * @tparam A Type of (primary) items in this group
 * @tparam Repr A type representation of this group
 */
trait SolvableGroup[A <: Solvable, +Repr] extends Solvable
{
	// ABSTRACT	--------------------------
	
	/**
	 * @return Solvable items in this group, in order
	 */
	def items: Vector[A]
	
	/**
	 * @return 'This' as 'Repr'
	 */
	def repr: Repr
	
	/**
	 * @param newItems New set of items
	 * @return A copy of this group with specified items
	 */
	def withItems(newItems: Vector[A]): Repr
	
	
	// IMPLEMENTED	----------------------
	
	override lazy val isSolved = items.forall { _.isSolved }
	
	
	// OTHER	--------------------------
	
	/**
	 * Tries to add some information to a single item in this group
	 * @param items Items to iterate over / solve
	 * @param solve A solving function that produces a new item and a change element (None if no change was made)
	 * @param makeCopy A function for producing a new copy of this group based on the modified items
	 * @tparam B Type of item being solved
	 * @tparam Change A change representation
	 * @tparam R Type of resulting group
	 * @return A modified copy of this group, along with the changed item and a change in that item
	 *         (this -> (item -> change in item)). None if there was no change.
	 */
	protected def trySolveNext[B <: Solvable, Change, R >: Repr](items: Vector[B])(solve: B => Option[(B, Change)])
																(makeCopy: Vector[B] => R): Option[(R, (B, Change))] =
	{
		if (isSolved)
			None
		else
		{
			// Finds the item that can be solved using the specified function
			items.findMapAndIndex { item =>
				if (item.isSolved)
					None
				else
					solve(item)
			}.map { result =>
				// Divides the results
				val (change, index) = result
				val newItem = change._1
				// Creates a new list of items and a new version of 'this'
				val newItems = items.updated(index, newItem)
				val newCopy = makeCopy(newItems)
				// Attaches information about the changed items
				newCopy -> change
			}
		}
	}
	
	/**
	 * Tries to add some information to a single item in this group
	 * @param solve A solving function that produces a new item and a change element (None if no change was made)
	 * @tparam Change A change representation
	 * @return A modified copy of this group, along with the changed item and a change in that item
	 *         (this -> (item -> change in item)). None if there was no change.
	 */
	def trySolveNext[Change](solve: A => Option[(A, Change)]): Option[(Repr, (A, Change))] = trySolveNext(items)(solve)(withItems)
	
	/**
	 * Tries to add some information to a single item in this group
	 * @param solve A solving function that produces a modified copy of an item (yields the same item when there is no change)
	 * @return New copy of this group + the modified item. None if no item was modified.
	 */
	def trySolveMapNext(solve: A => A): Option[(Repr, A)] = trySolveNext[A] { item =>
		// Test for change with an equality check
		val newItem = solve(item)
		if (newItem == item)
			None
		else
			Some(newItem -> newItem)
	}.map { result => result._1 -> result._2._1 } // Removes the duplicate item from 'change'
	
	/**
	 * Tries to add some information to this group in all items where it is possible
	 * @param items Items to iterate over / solve
	 * @param solve A solving function that produces a new item and a change (None if no change was made)
	 * @param makeCopy A function for producing a new copy of this group based on new information (called if some progress was made)
	 * @tparam B Type of item iterated over / solved
	 * @tparam Change A change representation
	 * @tparam R Type of resulting collection
	 * @return A new version of the group + a list of changes that were made. None if no changes were made.
	 */
	protected def trySolve[B <: Solvable, Change, R >: Repr](items: => Vector[B])(solve: B => Option[(B, Change)])
															(makeCopy: Vector[B] => R): Option[(R, Vector[Change])] =
	{
		if (isSolved)
			None
		else
		{
			// Keeps track of changes
			val changesBuilder = new VectorBuilder[Change]()
			// Solves items (skips those already solved)
			val newItems = items.map { item =>
				if (item.isSolved)
					item
				else
				{
					// Solve might not yield results
					solve(item) match
					{
						case Some(changeResult) =>
							changesBuilder += changeResult._2
							changeResult._1
						case None => item
					}
				}
			}
			// Checks whether any change was made & copies this group, if necessary
			val changes = changesBuilder.result()
			if (changes.isEmpty)
				None
			else
				Some(makeCopy(newItems) -> changes)
		}
	}
	
	/**
	 * Tries to add some information to all items in this group where it is possible
	 * @param solve A solving function that produces a new item and a change (None if no change was made)
	 * @tparam Change A change representation
	 * @return A new version of the group + a list of changes that were made. None if no changes were made.
	 */
	def trySolve[Change](solve: A => Option[(A, Change)]): Option[(Repr, Vector[Change])] = trySolve(items)(solve)(withItems)
	
	/**
	 * Tries to add some information to all items in this group where it is possible
	 * @param solve A solving function that produces a modified copy of an item (yields an equal item when there is no change)
	 * @return A new version of this group + list of changed items. None if no changes were made.
	 */
	def trySolveMap(solve: A => A): Option[(Repr, Vector[A])] = trySolve[A] { item =>
		// Checks for change by comparing items for equality
		val newItem = solve(item)
		if (newItem == item)
			None
		else
			Some(newItem -> newItem)
	}
}
