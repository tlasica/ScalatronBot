/**
 * Created by tomek on 12.01.15.
 */

case class Coord(row: Int, col: Int) {

  def isBackOf(m: Coord): Boolean =  {
    if (row == -m.row && col == -m.col) true else false
  }

  def findMoveTo(dest: Coord): Coord = {
    Coord(dest.row-this.row, dest.col-this.col)
  }

  def add(move: Coord): Coord = {
    new Coord(row + move.row, col + move.col)
  }
}

object Coord {
  def fromString(s: String) = {
    val c = s.split(":")
    Coord(c(0).toInt, c(1).toInt)
  }
}

abstract class ServerCommand

/**
 * "Welcome" is the first command sent by the server to a plug-in before any other invocations of the control function.
 * @param name player name
 * @param apocalypse the number of steps that will be performed in the upcoming game round.
 * @param round  the index of the round for which the control function was instantiated.
 * @param maxSlaves the number of slave bots that a user can have alive at any one time
 */
case class WelcomeCmd(name: String,
                   apocalypse: Int,
                   round: Int,
                   maxSlaves: Int) extends ServerCommand


case class ReactCmd(params: Map[String, String]) extends ServerCommand {

  def generation = params.getOrElse("generation", "0").toInt

  def name = params.getOrElse("name", "???")

  def time = params("time").toInt

  def energy = params("energy").toInt

  def masterPosition: Option[Coord] = params.get("master") match {
    case Some(x: String) => Some(Coord.fromString(x))
    case None => None
  }

  def collision: Option[Coord] = params.get("collision") match {
    case Some(x: String) => Some(Coord.fromString(x))
    case None => None
  }

  def view: Option[BotView] = params.get("view") match {
    case Some(x: String) => Some(BotView(x))
    case None => None
  }

  //TODO: view
  //TODO: slavesAlive
  //TODO: state

}

case class GoodbyeCmd(energy: Int) extends ServerCommand


