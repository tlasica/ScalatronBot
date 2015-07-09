import org.scalatest.{WordSpec, Matchers}

/**
 * Created by tomek on 31.01.15.
 */
class HarvesterMiniBotSpec extends WordSpec with Matchers {

  private[this] def reactWithView(view: String): ReactCmd = {
    val params = Map("view" -> view, "master"->"0:0", "time" -> "1")
    ReactCmd( params )
  }

  "Harvester Mini Bot" should {
    "avoid Snorg in corner with walls" in {
      val builder = new BotViewBuilder(21)
      builder.addRow(BotView.Wall, 9)
      builder.addCol(BotView.Wall, 9)
      builder.add(BotView.Snorg, 11, 10)
      val cmd = reactWithView( builder.createString )
      val bot = new HarvesterMiniBot(5000)
      val answer = bot.react( cmd )
      answer.head shouldBe MoveCommand(Coord(0,1))
      //println(answer.mkString(";"))
    }

    "escape from walls corner" in {
      val builder = new BotViewBuilder(21)
      builder.addRow(BotView.Wall, 9)
      builder.addCol(BotView.Wall, 9)
      val cmd = reactWithView( builder.createString )
      val bot = new HarvesterMiniBot(5000)
      val answer = bot.react( cmd )
      bot.isEscapeStarted shouldBe true
    }

  }
}
