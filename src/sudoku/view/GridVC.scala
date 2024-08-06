package sudoku.view

import sudoku.model.Grid
import utopia.flow.view.mutable.eventful.EventfulPointer
import utopia.firmament.component.display.RefreshableWithPointer
import utopia.firmament.context.ColorContext
import utopia.firmament.controller.data.ContainerContentDisplayer
import utopia.reflection.component.swing.template.StackableAwtComponentWrapperWrapper

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
	
	override val contentPointer = EventfulPointer[Grid](initialGrid)
	private val slotsPointer = contentPointer.map { _.slots }
	
	private val container = new GridContainer[SlotVC]
	
	
	// COMPUTED	-----------------------------
	
	/**
	 * @return Displays for all the slots managed by this Grid VC
	 */
	def slotDisplays = container.components
	
	
	// INITIAL CODE	--------------------------
	
	ContainerContentDisplayer.forImmutableStates(container, slotsPointer) { _.position == _.position } {
		slot => new SlotVC(slot) }
	
	parentContext.forTextComponents.use { implicit c => container.addCustomDrawer(borderSettings.gridBorderDrawer) }
	
	
	// IMPLEMENTED	--------------------------
	
	override protected def wrapped = container
}
