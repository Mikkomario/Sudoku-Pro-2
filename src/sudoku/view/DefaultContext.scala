package sudoku.view

import sudoku.model.BorderSettings
import utopia.firmament.context.BaseContext
import utopia.firmament.localization.{Localizer, NoLocalization}
import utopia.firmament.model.Margins
import utopia.flow.async.context.ThreadPool
import utopia.flow.util.logging.{Logger, SysErrLogger}
import utopia.genesis.handling.action.ActorHandler
import utopia.genesis.text.Font
import utopia.genesis.text.FontStyle.Plain
import utopia.paradigm.color.{ColorScheme, ColorSet}

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
	implicit val log: Logger = SysErrLogger
	implicit val exc: ExecutionContext = new ThreadPool("Sudoku Pro")
	
	val primaryColors = ColorSet.fromHexes("#e1f5fe", "#ffffff", "#afc2cb").get
	val secondaryColors = ColorSet.fromHexes("#ff6e40", "#ffa06d", "#c53d13").get
	val colorScheme = ColorScheme.twoTone(primaryColors, secondaryColors)
	
	val actorHandler = ActorHandler()
	val margins: Margins = Margins(16)
	
	val baseContext = BaseContext(actorHandler, Font("Roboto Condensed", 16, Plain, 2), colorScheme, margins)
	
	implicit val borderSettings: BorderSettings = BorderSettings(1, 0.5)
}
