import scala.collection.mutable

class ControlFunctionFactory {
  def create = new BotManager().respond _
}

class BotManager {

  case class BotInfo(time: Int, energy: Int)

  var master: Bot = _
  val miniBots = new scala.collection.mutable.HashMap[String, Bot]()
  val botsEnergy = new mutable.HashMap[String, BotInfo]
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
          botsEnergy.clear()
          List(new NullCommand)
        case r: ReactCmd =>
          if (r.generation==0) {
            master.react(r)
          }
          else {
            botsEnergy(r.name) = BotInfo(r.time, r.energy)
            r.name match {
              case x:String if x.startsWith("H") =>
                val bot = miniBots.getOrElseUpdate(x, new HarvesterMiniBot(apocalypse))
                bot.react(r)
              case x:String if x.startsWith("G") =>
                GuardianMiniBot.react(r)
            }
          }
        case g: GoodbyeCmd =>
          val minisEnergy = energyLeftAtApocalypse
          val msg = "Goodbye with energy %d and %d in remaining mini bots"format(g.energy, minisEnergy)
          println( msg )
          List(new NullCommand)
      }
      response.mkString("|")
    }
    catch {
      // FIXME: NonFatal will be a better option, but it is not supported in scala 2.9.x
      case e: Throwable =>
        println("Ops! > " + e.getMessage)
        e.printStackTrace()
    }
  }

  private[this] def energyLeftAtApocalypse: Long = {
    val livingBots = botsEnergy filter { case(_, info:BotInfo) => info.time == apocalypse }
    val energy = livingBots map { case (_, info:BotInfo) => info.energy }
    energy.sum
  }

}
