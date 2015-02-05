/**
 * Created by tomek on 12.01.15.
 */

case class Coord(row: Int, col: Int) {


  def opposite = Coord(-row, -col)

  def isOpposite(m: Coord): Boolean =  {
    if (row == -m.row && col == -m.col) true else false
  }

  def findMoveTo(dest: Coord): Coord = {
    Coord(dest.row-this.row, dest.col-this.col)
  }

  def findDirTo(dest: Coord): Coord = {
    val move = findMoveTo(dest)
    Coord(math.signum(move.row).toInt, math.signum(move.col).toInt)
  }

  def add(move: Coord): Coord = {
    new Coord(row + move.row, col + move.col)
  }

  def similarDirections: List[Coord] = {
    this match {
      case Coord(-1,-1) => List(Coord(-1,0), Coord(0,-1))
      case Coord(-1, 0) => List(Coord(-1,-1), Coord(0,-1))
      case Coord(-1, 1) => List(Coord(-1, 0), Coord(0, 1))
      case Coord(0, 1) => List(Coord(-1, 1), Coord(1, 1))
      case Coord(1, 1) => List(Coord(0, 1), Coord(1, 0))
      case Coord(1, 0) => List(Coord(1, -1), Coord(1, 1))
      case Coord(1, -1) => List(Coord(0, -1), Coord(-1, 0))
      case Coord(0, -1) => List(Coord(-1, -1), Coord(1, -1))
      case _ => List()
    }
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

  def generation: Int = params.getOrElse("generation", "0").toInt

  def name: String = params.getOrElse("name", "???")

  def time: Int = params("time").toInt

  def energy: Int = params.getOrElse("energy", "0").toInt

  def masterPosition: Option[Coord] = params.get("master") match {
    case Some(x: String) => Some(Coord.fromString(x))
    case None => None
  }

  def collision: Option[Coord] = params.get("collision") match {
    case Some(x: String) => Some(Coord.fromString(x))
    case None => None
  }

  def view: BotView = params.get("view") match {
    case Some(x: String) => BotView(x)
    case None => throw new IllegalArgumentException("No view string in the React() command")
  }

  //TODO: view
  //TODO: slavesAlive
  //TODO: state

}

case class GoodbyeCmd(energy: Int) extends ServerCommand


