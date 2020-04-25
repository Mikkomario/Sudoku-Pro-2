package sudoku.util

import scala.collection.immutable.VectorBuilder

/**
 * Used for building multi-maps
 * @author Mikko Hilpinen
 * @since 25.4.2020, v1
 */
class MultiMapBuilder[K, V]
{
	private var map = Map[K, VectorBuilder[V]]()
	
	def +=(key: K, value: V): Unit =
	{
		if (!map.contains(key))
			map += key -> new VectorBuilder[V]()
		
		map(key) += value
	}
	
	def +=(kv: (K, V)): Unit = +=(kv._1, kv._2)
	
	def ++=(key: K, values: Vector[V]): Unit =
	{
		if (!map.contains(key))
			map += key -> new VectorBuilder[V]()
		
		map(key) ++= values
	}
	
	def ++=(kv: (K, Vector[V])): Unit = ++=(kv._1, kv._2)
	
	def ++=(keys: IterableOnce[K], value: V) = keys.iterator.foreach { this += _ -> value }
	
	def result() = map.view.mapValues { _.result() }.toMap
}
