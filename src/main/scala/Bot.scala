/**
 * trait for bots so that they can be mixed-in
 * Created by tomek on 13.01.15.
 */
abstract class Bot {

  def react(reactCmd: ReactCmd): List[BotCommand] = {
    energy = reactCmd.energy
    timestamp = reactCmd.time
    List()
  }

  var energy: Int = 0
  var timestamp: Int = 0
}


class GoalFunDrivenBot extends Bot {

  this: DebugPrint =>

  var lastMove: Coord = Coord(0,0)

  private var escape = Escape(MasterPosition.coord)

  /**
   * If there are many near snorgs visible and no mini is already visible then
   * a new MiniBot is spawn in the direction oppostite to the next move of MasterBot
   */
  private def spawnGuardianIfsnorgsApproaching(cmd: ReactCmd, facts: List[ViewFact], move: MoveCommand) = {
    val snorgs = facts filter ( (f:ViewFact) => f.what == BotView.Snorg && f.distance<=5 )
    val minis = facts filter ( (f:ViewFact) => f.what == BotView.Mini && f.distance<10 )
    if (minis.isEmpty && snorgs.size>=3) {
      val dir = move.dir.opposite
      val energy = 133
      val name = "G%d".format(cmd.time)
      debug("Spawn: " + name)
      List( SpawnCommand(name, dir, energy))
    }
    else List()
  }

  private def spawnHarvesterMiniBot(cmd: ReactCmd, facts: List[ViewFact], move: MoveCommand) = {
    val fluppets = facts filter ((f:ViewFact) => f.what == BotView.Fluppet)
    val zugars = facts filter ((f:ViewFact) => f.what == BotView.Zugar)
    val nearMinis = facts filter ( (f:ViewFact) => f.what == BotView.Mini && f.distance<8 )
    val isWorth = fluppets.size + zugars.size > 0
    if (isWorth && nearMinis.isEmpty) {
      val dir = move.dir.opposite
      val energy = 100
      val name = "H%d".format(cmd.time)
      List( SpawnCommand(name, dir, energy))
    }
    else List()
  }


  override def react(reactCmd: ReactCmd): List[BotCommand] = {
    val facts = reactCmd.view.factsWithDistance(MasterPosition.coord)
    val esc = escape.move(reactCmd.view)
    val move =
    if (esc.isDefined) {
      esc.get
    }
    else {
      moveForBestValue(reactCmd, facts)
    }
    val spawnGuards = spawnGuardianIfsnorgsApproaching(reactCmd, facts, move )
    val spawnHarvesters = spawnHarvesterMiniBot(reactCmd, facts, move )
    move::spawnGuards:::spawnHarvesters
  }

  def moveForBestValue(reactCmd: ReactCmd, facts: List[ViewFact]): MoveCommand = {
    //println( statusString(reactCmd) )

    val view = reactCmd.view
    var bestValue = GoalValue.forView( view, MasterPosition.coord )
    debug("Current val:" + bestValue)
    val neighbours = Distance.neighbours(MasterPosition.row, MasterPosition.col, 31)
    var bestMove = Coord(0, 0)  // do not move at all
    val availableNeighbours = neighbours filter ( (x:(Int, Int)) => view.at(x._1, x._2) != BotView.Wall)
    for(n <- availableNeighbours) {
      val newPos:Coord = Coord(n._1, n._2)
      val move = MasterPosition.coord.findMoveTo(newPos)
      if (! move.isOpposite(lastMove)) {
        val value = GoalValue.forView(view, newPos)
        debug( "move by " + move + " will lead to " + value + " and situation:")
        debug( GoalValue.situation(view, newPos.row, newPos.col).mkString("\n"))
        if (value > bestValue) {
          bestMove = move
          bestValue = value
        }
      }
    }

    // if decided to stay and not to move we switch to escape mode
    val noMoveFound = bestMove == Coord(0,0)
    val nothingInteresting = facts.isEmpty
    if (noMoveFound || nothingInteresting) {
      val escapeDir = prepareEscape(reactCmd.view)
      val escapeSteps = 17
      escape.start(escapeDir, escapeSteps)
      bestMove = Coord(0,0)
    }

    lastMove = bestMove
    debug("decision: " + bestMove)
    printDebug()
    MoveCommand(bestMove)
  }

  private def prepareEscape(view: BotView): Coord = {
    val (bestDir, bestVis) = Distance.mostVisibleDirection(view, MasterPosition.coord)
    bestDir
  }

  private def statusString(cmd: ReactCmd) : String = {
    "time:%d, energy:%d".format(cmd.time, cmd.energy)
  }

}
