package sudoku.view

import sudoku.model.BorderSettings
import utopia.flow.async.ThreadPool
import utopia.genesis.color.Color
import utopia.genesis.handling.mutable.ActorHandler
import utopia.reflection.color.{ColorScheme, ColorSet}
import utopia.reflection.component.context.BaseContext
import utopia.reflection.localization.{Localizer, NoLocalization}
import utopia.reflection.shape.Margins
import utopia.reflection.text.Font
import utopia.reflection.text.FontStyle.Plain

import scala.concurrent.ExecutionContext

/**
 * This object contains some global settings used in views
 * @author Mikko Hilpinen
 * @since 29.4.2020, v1
 */
object DefaultContext
{
	// Sets up localization context
	implicit val localizer: Localizer = NoLocalization
	
	// Creates component context
	implicit val exc: ExecutionContext = new ThreadPool("Sudoku Pro").executionContext
	
	val primaryColors = ColorSet.fromHexes("#e1f5fe", "#ffffff", "#afc2cb").get
	val secondaryColors = ColorSet.fromHexes("#ff6e40", "#ffa06d", "#c53d13").get
	val colorScheme = ColorScheme(primaryColors, secondaryColors)
	
	val actorHandler = ActorHandler()
	val margins: Margins = Margins(16)
	
	val baseContext = BaseContext(actorHandler, Font("Roboto Condensed", 16, Plain, 2), colorScheme, margins)
	
	implicit val borderSettings: BorderSettings = BorderSettings(1, 0.5)
}
