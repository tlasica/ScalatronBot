abstract class BotCommand

case class MoveCommand(dir: Coord) extends BotCommand {
  override def toString: String = "Move(direction=%d:%d)".format(dir.col, dir.row)
}

case class SpawnCommand(name: String, dir: Coord, energy: Int) extends BotCommand {
  override def toString: String = "Spawn(name=%s,direction=%d:%d,energy=%d)".format(name, dir.col, dir.row, energy)
}

class NullCommand extends BotCommand {
  override def toString: String = ""
}

case class ExplodeCommand(radius: Int) extends BotCommand {
  require(radius>=2 && radius<=10)
  override def toString: String = "Explode(size=%d)".format(radius)
}

case class StatusCommand(text: String) extends BotCommand {
  override def toString: String = "Status(text=%s)".format(text)
}
