package sudoku.view

import sudoku.model.Grid
import utopia.flow.datastructure.mutable.PointerWithEvents
import utopia.reflection.component.RefreshableWithPointer
import utopia.reflection.component.context.ColorContext
import utopia.reflection.component.swing.StackableAwtComponentWrapperWrapper
import utopia.reflection.controller.data.ContainerContentManager

/**
 * Displays a sudoku grid
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
class GridVC(initialGrid: Grid)(implicit parentContext: ColorContext)
	extends StackableAwtComponentWrapperWrapper with RefreshableWithPointer[Grid]
{
	import DefaultContext._
	
	// ATTRIBUTES	--------------------------
	
	override val contentPointer = new PointerWithEvents[Grid](initialGrid)
	private val container = new GridContainer[SlotVC]
	
	private val manager = ContainerContentManager.forImmutableStates(container, initialGrid.slots) { _.position == _.position } {
		slot => new SlotVC(slot) }
	
	
	// COMPUTED	-----------------------------
	
	/**
	 * @return Displays for all the slots managed by this Grid VC
	 */
	def slotDisplays = container.components
	
	
	// INITIAL CODE	--------------------------
	
	contentPointer.addListener { e => manager.content = e.newValue.slots }
	parentContext.forTextComponents().use { implicit c => container.addCustomDrawer(borderSettings.gridBorderDrawer) }
	
	
	// IMPLEMENTED	--------------------------
	
	override protected def wrapped = container
}
