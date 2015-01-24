import org.scalatest.{WordSpec, Matchers}

/**
 * Created by tomek on 24.01.15.
 */
class ValueDrivenBotSpec extends WordSpec with Matchers {

  private def reactWithView(view: String): ReactCmd = {
    val params = Map("view" -> view)
    ReactCmd( params )
  }

  "Value-Driven Bot" should {
    "avoid Snorg in corner with walls" in {
      val builder = new BotViewBuilder(31)
      for(i <- Range(0,31)) {
        builder.add(BotView.Wall, 0, i)
        builder.add(BotView.Wall, i, 0)
      }
      builder.add(BotView.Snorg, 15, 17)
      val bot = new GoalFunDrivenBot with SimpleDebugPrint
      val cmd = reactWithView( builder.createString )
      val move = bot.react( cmd )
    }
  }

}
