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

  import scala.collection.mutable.Queue

  var lastMove: Coord = Coord(0,0)

  override def react(reactCmd: ReactCmd): String = {

    def isAcceptable(m: Coord): Boolean = {
      if (m.isBackOf(lastMove)) false
      else reactCmd.view match {
        case Some(v) =>
          val cell = v.at(m.row, m.col)
          BotView.isSafe(cell)
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

  var escape = new scala.collection.mutable.Queue[Coord]

  override def react(reactCmd: ReactCmd): String = {
    val cmd =
    if (escape.nonEmpty) {
      val move = escape.dequeue()
      moveCmd(move)
    }
    else {
      moveForBestValue(reactCmd)
    }
    cmd
  }

  def moveForBestValue(reactCmd: ReactCmd): String = {
    val cmd = reactCmd.view match {
      case Some(view) =>
        val debug = new StringBuilder
        var bestValue = GoalValue.forView( view, BotView.MasterPos )
        debug ++= "current val:" + bestValue + "\n"
        val sit = GoalValue.situation(view, 15, 15)
        val nbours = Distance.neighbours(15, 15, 31)
        var bestMove = Coord(0, 0)
        val availabeNBours = nbours filter ( (x:(Int, Int)) => view.at(x._1, x._2) != BotView.Wall)
        for(n <- availabeNBours) {
          val newPos:Coord = Coord(n._1, n._2)
          val move = BotView.MasterPos.findMoveTo(newPos)
          //println("considering move by " + move + " to " + newPos)
          if (! move.isBackOf(lastMove)) {
            val value = GoalValue.forView(view, newPos)
            debug ++= "move by " + move + " will lead to " + value + "\n"
            debug ++= "-> " + GoalValue.situation(view, newPos.row, newPos.col) + "\n"
            if (value >= bestValue) {
              bestMove = move
              bestValue = value
            }
          }
        }
        // if decided to stay and not to move we switch to escape mode
        if (bestMove == Coord(0,0)) {
          println("Decided to not move")
          println(view.toPrettyString())
          println("situation: " + sit)
          println(debug.toString())
          // prepare escape and do the 1st move
          val escapeRoute = prepareEscape(view)
          escape ++= escapeRoute
          moveCmd( escape.dequeue() )
        }
        //println("val:" + bestValue + " move:" +bestMove)
        lastMove = bestMove
        moveCmd(bestMove)

      case None => ""
    }
    cmd
  }

  def prepareEscape(view: BotView): List[Coord] = {
    def visibility(from: Coord, dir: Coord): Int = {
      var newPos = from.add(dir)
      var vis = 0
      while (view.isPositionCorrect(newPos) && BotView.isSafe(view.at(newPos)) ) {
        vis += 1
        newPos = newPos.add(dir)
      }
      vis
    }

    val directions = Distance.directions
    val visibilities = directions map (d => (visibility(Coord(15,15), d), d) )

    val maxVis = (visibilities map ((x:(Int, Coord)) => x._1)).max
    val bestDir = (visibilities filter ((x:(Int, Coord)) => x._1 == maxVis)).head

    List.fill(bestDir._1 max 6)(bestDir._2)
  }

}
