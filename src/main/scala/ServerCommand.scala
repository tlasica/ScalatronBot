/**
 * Created by tomek on 12.01.15.
 */

case class Coord(x: Int, y: Int)

abstract class ServerCommand {
}

object ServerCommand {
  type BotState = Map[String, String]
}

/**
 * "Welcome" is the first command sent by the server to a plug-in before any other invocations of the control function.
 * @param name player name
 * @param apocalypse the number of steps that will be performed in the upcoming game round.
 * @param round  the index of the round for which the control function was instantiated.
 * @param maxSlaves the number of slave bots that a user can have alive at any one time
 */
case class Welcome(name: String,
                   apocalypse: Int,
                   round: Int,
                   maxSlaves: Int) extends ServerCommand


case class React(generation: Int,
                 name:String,
                 time:Int,
                 view: String,
                 energy: String,
                 masterPos: Coord,
                 collision: Coord,
                 slaves: Int,
                  state: ServerCommand.BotState) extends ServerCommand

case class Goodbye(energy: Int) extends ServerCommand
