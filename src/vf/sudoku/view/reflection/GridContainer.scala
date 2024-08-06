package vf.sudoku.view.reflection

import utopia.firmament.component.AreaOfItems
import utopia.firmament.drawing.mutable.MutableCustomDrawableWrapper
import utopia.firmament.model.stack.{StackLength, StackSize}
import utopia.paradigm.shape.shape2d.area.polygon.c4.bounds.Bounds
import utopia.paradigm.shape.shape2d.vector.Vector2D
import utopia.paradigm.shape.shape2d.vector.point.Point
import utopia.reflection.component.swing.template.{AwtComponentWrapperWrapper, SwingComponentRelated}
import utopia.reflection.component.template.layout.stack.CachingReflectionStackable
import utopia.reflection.container.stack.template.MultiStackContainer
import utopia.reflection.container.swing.layout.multi.Stack.AwtStackable
import utopia.reflection.container.swing.{AwtContainerRelated, Panel}

/**
 * Displays components as a symmetric grid
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
class GridContainer[C <: AwtStackable] extends MultiStackContainer[C]
	with AwtComponentWrapperWrapper with AwtContainerRelated with SwingComponentRelated with MutableCustomDrawableWrapper
	with CachingReflectionStackable with AreaOfItems[C]
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
			comps(i).bounds = Bounds(componentSize.toPoint * Vector2D(x, y), componentSize)
		}
	}
	
	override protected def updateVisibility(visible: Boolean) = super[AwtComponentWrapperWrapper].visible_=(visible)
	
	override def calculatedStackSize =
	{
		// Each component will be scaled to similar symmetric size so the longest stack length is used
		val lengths = components.flatMap { _.stackSize.dimensions }
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
	
	override protected def addToContainer(component: C, index: Int): Unit = panel.insert(component, index)
	override protected def removeFromContainer(component: C): Unit = panel -= component
	
	override def components = panel.components
}
