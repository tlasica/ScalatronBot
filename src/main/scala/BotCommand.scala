/**
 * Created by tomek on 24.01.15.
 */
abstract class BotCommand {

}

case class MoveCommand(dir: Coord) extends BotCommand {
  override def toString = "Move(direction=%d:%d)".format(dir.col, dir.row)
}

case class SpawnCommand(dir: Coord, energy: Int) extends BotCommand {
  override def toString = "Spawn(direction=%d:%d,energy=%d)".format(dir.col, dir.row, energy)
}

class NullCommand extends BotCommand {
  override def toString = ""
}

case class ExplodeCommand(radius: Int) extends BotCommand {
  require(radius>=2 && radius<=10)
  override def toString = "Explode(size=%d)".format(radius)
}