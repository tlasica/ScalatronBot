/**
 * Created by tomek on 11.01.15.
 */
class ControlFunctionFactory {
  def create = new BotManager().respond _
}

class BotManager {

  var master: Bot = _
  val miniBots = new scala.collection.mutable.HashMap[String, Bot]()

  def respond(input: String) = {
    try {
      val cmd = ServerCommandParser.parse(input)
      val response: List[BotCommand] = cmd match {
        case w: WelcomeCmd =>
          println("New round " + w.round.toString + " started")
          master = new GoalFunDrivenBot with NoDebugPrint
          miniBots.clear()
          List(new NullCommand)
        case r: ReactCmd =>
          if (r.generation==0) {
            master.react(r)
          }
          else {
            r.name match {
              case x:String if x.startsWith("H") =>
                val bot = miniBots.getOrElseUpdate(x, new HarvesterMiniBot)
                bot.react(r)
              case x:String if x.startsWith("G") =>
                GuardianMiniBot.react(r)
            }
          }
        case g: GoodbyeCmd =>
          println("Goodbye with energy:" + g.energy.toString)
          List(new NullCommand)
      }
      response.mkString("|")
    }
    catch {
      case e:Throwable =>
        println("Fuck! " + e.getMessage())
        e.printStackTrace
             // TODO: Logging
    }
  }

  private def getOrCreateHarvester(name: String) = {
    miniBots.getOrElseUpdate(name, new HarvesterMiniBot)
  }

}
