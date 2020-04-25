package sudoku.view

import scala.math.Ordering.Double.TotalOrdering

import utopia.genesis.shape.shape2D.{Bounds, Point}
import utopia.reflection.component.AreaOfItems
import utopia.reflection.component.drawing.mutable.CustomDrawableWrapper
import utopia.reflection.component.stack.CachingStackable
import utopia.reflection.component.swing.{AwtComponentWrapperWrapper, SwingComponentRelated}
import utopia.reflection.container.stack.MultiStackContainer
import utopia.reflection.container.swing.{AwtContainerRelated, Panel}
import utopia.reflection.container.swing.Stack.AwtStackable
import utopia.reflection.shape.{StackLength, StackSize}

/**
 * Displays components as a symmetric grid
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
class GridContainer[C <: AwtStackable] extends MultiStackContainer[C]
	with AwtComponentWrapperWrapper with AwtContainerRelated with SwingComponentRelated with CustomDrawableWrapper
	with CachingStackable with AreaOfItems[C]
{
	// ATTRIBUTES	----------------------------
	
	private val panel = new Panel[C]
	
	
	// COMPUTED	--------------------------------
	
	private def lineLength = math.ceil(math.sqrt(components.size)).toInt
	
	
	// IMPLEMENTED	----------------------------
	
	override def areaOf(item: C) = Some(item.bounds)
	
	override def itemNearestTo(relativePoint: Point) = components.minByOption { c =>
		c.bounds.center.distanceFrom(relativePoint)
	}
	
	override def children = super[MultiStackContainer].children
	
	override def component = panel.component
	
	override protected def wrapped = panel
	
	override def drawable = panel
	
	override def updateLayout() =
	{
		// Places the items in a grid, all with the same size
		val comps = components
		val componentsPerLine = lineLength
		val componentSize = size / componentsPerLine
		comps.indices.foreach { i =>
			val x = i % componentsPerLine
			val y = i / componentsPerLine
			comps(i).bounds = Bounds(componentSize.toPoint * (x, y), componentSize)
		}
	}
	
	override protected def updateVisibility(visible: Boolean) = super[AwtComponentWrapperWrapper].isVisible_=(visible)
	
	override def calculatedStackSize =
	{
		// Each component will be scaled to similar symmetric size so the longest stack length is used
		val lengths = components.flatMap { _.stackSize.components }
		if (lengths.isEmpty)
			StackSize.any
		else
		{
			val largestMin = lengths.map { _.min }.max
			val smallestMax = lengths.flatMap { _.max }.minOption
			val largestOptimal = lengths.map { _.optimal }.max
			
			val optimal = smallestMax.filter { _ < largestOptimal } match
			{
				case Some(affectingMax) =>
					if (affectingMax < largestMin)
						largestMin
					else
						affectingMax
				case None => largestOptimal
			}
			
			(StackLength(largestMin, optimal, smallestMax) * lineLength).square
		}
	}
	
	override protected def add(component: C, index: Int) = panel.insert(component, index)
	
	override protected def remove(component: C) = panel -= component
	
	override def components = panel.components
}
