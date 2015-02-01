/**
 * Created by tomek on 11.01.15.
 */
class ControlFunctionFactory {
  def create = new BotManager().respond _
}

class BotManager {

  var master: Bot = _
  val miniBots = new scala.collection.mutable.HashMap[String, Bot]()
  var apocalypse: Int = _

  def respond(input: String) = {
    try {
      val cmd = ServerCommandParser.parse(input)
      val response: List[BotCommand] = cmd match {
        case w: WelcomeCmd =>
          apocalypse = w.apocalypse
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
                val bot = miniBots.getOrElseUpdate(x, new HarvesterMiniBot(apocalypse))
                bot.react(r)
              case x:String if x.startsWith("G") =>
                GuardianMiniBot.react(r)
            }
          }
        case g: GoodbyeCmd =>
          val livingMinis = miniBots.filter( (x:(String,Bot)) => x._2.timestamp == apocalypse )
          val minisEnergy = miniBots map ( (x:(String,Bot)) => x._2.energy ) sum
          val msg = "Goodbye with energy %d and %d in remaining mini bots"format(g.energy, minisEnergy)
          println( msg )
          List(new NullCommand)
      }
      response.mkString("|")
    }
    catch {
      case e:Throwable =>
        println("Fuck! " + e.getMessage)
        e.printStackTrace()
    }
  }

}
