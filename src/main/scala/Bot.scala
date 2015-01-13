/**
 * trait for bots so that they can be mixed-in
 * Created by tomek on 13.01.15.
 */
class Bot {

  def react(reactCmd: ReactCmd): String = ""

  def stop = ""

  def moveCmd(coord: Coord): String = "Move(direction=" + coord.x  + ":" + coord.y + ")"

}


class RandomMoveBot extends Bot {

  var lastMove: Coord = Coord(0,0)

  override def react(reactCmd: ReactCmd): String = {

    def isAcceptable(m: Coord): Boolean = {
      if (m.x == lastMove.x && m.y == lastMove.y) false
      else reactCmd.view match {
        case Some(v) => v.at(m.x, m.y) match {
          case BotView.Wall => false
          case BotView.EnemyMaster => false
          case BotView.Toxifera => false
          case BotView.Snorg => false
          case _ => true
        }
        case None => true
      }
    }

    val move = randomMove
    if (isAcceptable(move)) {
      lastMove = move
      moveCmd(move)
    }
    else react(reactCmd)  // try again
  }

  private def randomMove: Coord = {
    val dx = scala.util.Random.nextInt(3)-1
    val dy = scala.util.Random.nextInt(3)-1
    Coord(dx, dy)
  }
}