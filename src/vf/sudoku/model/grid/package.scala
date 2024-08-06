package vf.sudoku.model

import utopia.paradigm.shape.template.Dimensions

/**
  * @author Mikko Hilpinen
  * @since 05.08.2024, v1.1
  */
package object grid
{
	/**
	  * A grid coordinate type
	  */
	type Coordinate = Dimensions[Int]
	
	/**
	  * @return Factory for constructing grid coordinates
	  */
	def Coordinate = Dimensions.int
}
