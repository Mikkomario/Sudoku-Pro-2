package sudoku.view

import sudoku.model.{BorderSettings, Grid}
import utopia.flow.datastructure.mutable.PointerWithEvents
import utopia.reflection.component.RefreshableWithPointer
import utopia.reflection.component.swing.StackableAwtComponentWrapperWrapper
import utopia.reflection.controller.data.ContainerContentManager
import utopia.reflection.shape.Margins
import utopia.reflection.util.ComponentContextBuilder

/**
 * Displays a sudoku grid
 * @author Mikko Hilpinen
 * @since 22.4.2020, v1
 */
class GridVC(initialGrid: Grid)(implicit baseCB: ComponentContextBuilder, margins: Margins, borderSettings: BorderSettings)
	extends StackableAwtComponentWrapperWrapper with RefreshableWithPointer[Grid]
{
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
	container.addCustomDrawer(borderSettings.gridBorderDrawer)
	
	
	// IMPLEMENTED	--------------------------
	
	override protected def wrapped = container
}
