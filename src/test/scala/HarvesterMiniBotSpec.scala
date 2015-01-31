import org.scalatest.{WordSpec, Matchers}

/**
 * Created by tomek on 31.01.15.
 */
class HarvesterMiniBotSpec extends WordSpec with Matchers {

  private def reactWithView(view: String): ReactCmd = {
    val params = Map("view" -> view, "master"->"0:0")
    ReactCmd( params )
  }

  "Harvester Mini Bot" should {
    "avoid Snorg in corner with walls" in {
      val builder = new BotViewBuilder(21)
      for(i <- Range(0,21)) {
        builder.add(BotView.Wall, 9, i)
        builder.add(BotView.Wall, i, 9)
      }
      builder.add(BotView.Snorg, 11, 10)
      val cmd = reactWithView( builder.createString )
      val bot = new HarvesterMiniBot
      val answer = bot.react( cmd )
      answer.head shouldBe MoveCommand(Coord(0,1))
      println(answer.mkString(";"))
    }

    "escape from walls corner" in {
      val builder = new BotViewBuilder(21)
      for(i <- Range(0, 21)) {
        builder.add(BotView.Wall, 9, i)
        builder.add(BotView.Wall, i, 9)
      }
      val cmd = reactWithView( builder.createString )
      val bot = new HarvesterMiniBot
      val answer = bot.react( cmd )
      //answer.head shouldBe MoveCommand(Coord(0,1))
      println(answer.mkString(";"))
    }

  }
}
