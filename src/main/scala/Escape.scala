/**
 * Created by tomek on 01.02.15.
 */
case class Escape(pos: Coord) {

  private var escapeDir: Option[Coord] = None
  private var escapeSteps: Int = 0

  def start(dir:Coord, steps:Int): Unit = {
    escapeDir = Some(dir)
    escapeSteps = steps
  }

  private def stop(): Unit = {
    escapeDir = None
    escapeSteps = 0
  }

  def move(view: BotView): Option[MoveCommand] = {
    var res: Option[MoveCommand] = None
    if (escapeDir.isDefined) {
      val dest = pos add escapeDir.get
      if ( escapeSteps>0 && view.at(dest) == BotView.Empty ) {
        res = nextMove
      }
      else stop()
    }
    res
  }

  private def nextMove: Option[MoveCommand] = {
    require(escapeSteps > 0 && escapeDir.isDefined)
    escapeSteps = escapeSteps - 1
    Some(MoveCommand(escapeDir.get))
  }

}
