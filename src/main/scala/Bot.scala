/**
 * trait for bots so that they can be mixed-in
 * Created by tomek on 13.01.15.
 */
abstract class Bot {

  def react(reactCmd: ReactCmd): List[BotCommand]

}

object Bot {

}
class GoalFunDrivenBot extends Bot {

  this: DebugPrint =>

  var lastMove: Coord = Coord(0,0)

  var escape = new scala.collection.mutable.Queue[Coord]

  def spawnGuardianIfsnorgsApproaching(cmd: ReactCmd, move: MoveCommand) = {
    cmd.view match {
      case Some(view) =>
        val facts = view.factsWithDistance(MasterPosition.coord)
        val snorgs = facts filter ( (f:ViewFact) => f.what == BotView.Snorg && f.distance<=5 )
        val minis = facts filter ( (f:ViewFact) => f.what == BotView.Snorg && f.distance<10 )
        if (minis.isEmpty && snorgs.size>=3) {
          val dir = move.dir.opposite
          List( SpawnCommand(dir, 133))
        }
        else List()

      case None => List()
    }
  }

  override def react(reactCmd: ReactCmd): List[BotCommand] = {
    val cmd: List[BotCommand] =
    if (escape.nonEmpty) {
      val move = escape.dequeue()
      lastMove = move
      List(MoveCommand(move), StatusCommand("escape!"))
    }
    else {
      val moves = List( moveForBestValue(reactCmd) )
      val spawn = spawnGuardianIfsnorgsApproaching(reactCmd, moves.head )
      StatusCommand("")::(moves:::spawn)
    }
    cmd
  }

  def moveForBestValue(reactCmd: ReactCmd): MoveCommand = {
    val move = reactCmd.view match {
      case Some(view) =>
        var bestValue = GoalValue.forView( view, MasterPosition.coord )
        debug("current val:" + bestValue)
        val sit = GoalValue.situation(view, MasterPosition.row, MasterPosition.col)
        val neighbours = Distance.neighbours(MasterPosition.row, MasterPosition.col, 31)
        var bestMove = Coord(0, 0)
        val availableNeighbours = neighbours filter ( (x:(Int, Int)) => view.at(x._1, x._2) != BotView.Wall)
        for(n <- availableNeighbours) {
          val newPos:Coord = Coord(n._1, n._2)
          val move = MasterPosition.coord.findMoveTo(newPos)
          //println("considering move by " + move + " to " + newPos)
          if (! move.isOpposite(lastMove)) {
            val value = GoalValue.forView(view, newPos)
            debug( "move by " + move + " will lead to " + value + " and situation:")
            debug( GoalValue.situation(view, newPos.row, newPos.col).mkString("\n"))
            if (value >= bestValue) {
              bestMove = move
              bestValue = value
            }
          }
        }
        // print debug info
        debug("=======================")
        debug(sit.mkString("\n"))
        // if decided to stay and not to move we switch to escape mode
        if (bestMove == Coord(0,0) /*|| (bestValue>0 && bestValue<1000) */) {
          debug("Entering ESCAPE mode")
          // prepare escape and do the 1st move
          val escapeRoute = prepareEscape(view)
          escape ++= escapeRoute
          escape.dequeue()
        }
        else {
          bestMove
        }

      case None => Coord(0,0)
    }
    lastMove = move
    debug("decision: " + move)
    printDebug()

    MoveCommand(move)
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

    List.fill(bestDir._1 max 3)(bestDir._2)
  }

}
