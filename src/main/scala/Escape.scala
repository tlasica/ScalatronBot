/**
 * Created by tomek on 01.02.15.
 */
case class Escape(pos: Coord) {

  private[this] var escapeDir: Option[Coord] = None
  private[this] var escapeSteps: Int = 0

  def start(dir:Coord, steps:Int): Unit = {
    escapeDir = Some(dir)
    escapeSteps = steps
  }

  private[this] def stop(): Option[MoveCommand] = {
    escapeDir = None
    escapeSteps = 0
    None
  }

  def move(view: BotView): Option[MoveCommand] = escapeDir match {
    case Some(move) =>
      val dest = pos add move
      val isDestFree = view.at(dest) == BotView.Empty
      if (escapeSteps > 0 && isDestFree )  nextMove()
      else stop()
    case None => None

  }

  private[this] def nextMove(): Option[MoveCommand] = {
    require(escapeSteps > 0 && escapeDir.isDefined)
    escapeSteps = escapeSteps - 1
    Some(MoveCommand(escapeDir.get))
  }

}
