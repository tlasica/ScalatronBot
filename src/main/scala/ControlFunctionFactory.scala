/**
 * Created by tomek on 11.01.15.
 */
class ControlFunctionFactory {
  def create = new BotManager().respond _
}

// TODO: it may be a good choice to let master pass react to it's mini bots (manager should be eliminated)

class BotManager {

  var master: Bot = new Bot

  def respond(input: String) = {
    try {
      val cmd = ServerCommandParser.parse(input)
      cmd match {
        case w: WelcomeCmd =>
          println("New round " + w.round.toString + " started")
          master = new GoalFunDrivenBot with NoDebugPrint
          ""
        case r: ReactCmd =>
          if (r.generation==0) master.react(r)
          else "" // TODO: Przydupas
        case g: GoodbyeCmd =>
          println("Goodbye with energy:" + g.energy.toString)
          master stop
      }
    }
    catch {
      case e:Throwable =>
        println("Fuck! " + e.getMessage())
        e.printStackTrace
             // TODO: Logging
    }
  }


}
