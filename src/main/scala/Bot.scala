/**
 * trait for bots so that they can be mixed-in
 * Created by tomek on 13.01.15.
 */
class Bot {

  def react(reactCmd: ReactCmd): String = ""

  def stop = ""

  // coordinates in Moves are (x, y) from top left
  def moveCmd(coord: Coord): String = "Move(direction=" + coord.col  + ":" + coord.row + ")"



}

object Bot {
  def isBackMove(last: Coord, curr: Coord) : Boolean = {
    if (curr.row == -last.row && curr.col == -last.col) true else false
  }

}

class RandomMoveBot extends Bot {

  var lastMove: Coord = Coord(0,0)

  override def react(reactCmd: ReactCmd): String = {

    def isAcceptable(m: Coord): Boolean = {
      if (m.isBackOf(lastMove)) false
      else reactCmd.view match {
        case Some(v) => v.at(m.row, m.col) match {
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

class GoalFunDrivenBot extends Bot {

  var lastMove: Coord = Coord(0,0)

  override def react(reactCmd: ReactCmd): String = {
    val cmd = reactCmd.view match {
      case Some(view) =>
        //println("starting...")
        val gfunVal = GoalValue.forView( view, BotView.MasterPos )
        //println("current val:" + gfunVal)
        val sit = GoalValue.situation(view, 15, 15)
        //println(sit.mkString("\n"))
        val nbours = Distance.neighbours(15, 15, 31)
        var bestValue = gfunVal
        var bestMove = Coord(0, 0)
        val availabeNBours = nbours filter ( (x:(Int, Int)) => view.at(x._1, x._2) != BotView.Wall)
        for(n <- availabeNBours) {
          val newPos:Coord = Coord(n._1, n._2)
          val move = BotView.MasterPos.moveTo(newPos)
          //println("considering move by " + move + " to " + newPos)
          if (! move.isBackOf(lastMove)) {
            val value = GoalValue.forView(view, newPos)
            //println("..move by " + move + " will lead to " + value)
            if (value >= bestValue) {
              bestMove = move
              bestValue = value
            }
          }
        }
        //println("val:" + bestValue + " move:" +bestMove)
        lastMove = bestMove
        moveCmd(bestMove)

      case None => ""
    }
    cmd
  }

}