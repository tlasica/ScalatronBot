/**
 * trait for bots so that they can be mixed-in
 * Created by tomek on 13.01.15.
 */
abstract class Bot {

  def react(reactCmd: ReactCmd): List[BotCommand] = {
    List()
  }

}


class GoalFunDrivenBot extends Bot {

  this: DebugPrint =>

  var lastMove: Coord = Coord(0,0)

  private[this] val escape = Escape(MasterPosition.coord)

  /**
   * If there are many near snorgs visible and no mini is already visible then
   * a new MiniBot is spawn in the direction oppostite to the next move of MasterBot
   */
  private[this] def spawnGuardianIfsnorgsApproaching(cmd: ReactCmd, facts: List[ViewFact], move: MoveCommand) = {
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

  private[this] def spawnHarvesterMiniBot(cmd: ReactCmd, facts: List[ViewFact], move: MoveCommand) = {
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

    def bestMove(destinations: List[Coord], bestValSoFar: Long, bestMoveSoFar: Coord): Coord = destinations match {
      case List() => bestMoveSoFar
      case d :: rest =>
        val move = MasterPosition.coord.findMoveTo(d)
        if (! move.isOpposite(lastMove)) {
          val value = GoalValue.forView(reactCmd.view, d)
          debug( "move by " + move + " will lead to " + value + " and situation:")
          debug( GoalValue.situation(reactCmd.view, d.row, d.col).mkString("\n"))
          val betterVal = value max bestValSoFar
          val betterMove = if (value > bestValSoFar) move else bestMoveSoFar
          bestMove(rest, betterVal, betterMove)
        }
        else bestMove(rest, bestValSoFar, bestMoveSoFar)
    }

    val currValue = GoalValue.forView( reactCmd.view, MasterPosition.coord )
    debug("Current val:" + currValue)
    val neighbours = Distance.neighbours(MasterPosition.row, MasterPosition.col, BotView.MasterViewSize)
    val availableNeighbours = neighbours filter { case (row, col) => reactCmd.view.at(row, col) != BotView.Wall }
    val destCoords = availableNeighbours map { case (row, col) => Coord(row, col) }
    val best = bestMove(destCoords, currValue, Coord(0,0))

    // if decided to stay and not to move we switch to escape mode
    val noMoveFound = best == Coord(0, 0)
    val nothingInteresting = facts.isEmpty
    if (noMoveFound || nothingInteresting) {
      val escapeDir = prepareEscape(reactCmd.view)
      val escapeSteps = 17
      escape.start(escapeDir, escapeSteps)
      printDebug()
      lastMove = Coord(0, 0)
      MoveCommand(Coord(0,0))
    }
    else {
      printDebug()
      lastMove = best
      MoveCommand(best)
    }
  }

  private[this] def prepareEscape(view: BotView): Coord = {
    val (bestDir, bestVis) = Distance.mostVisibleDirection(view, MasterPosition.coord)
    bestDir
  }

  private[this] def statusString(cmd: ReactCmd) : String = {
    "time:%d, energy:%d".format(cmd.time, cmd.energy)
  }

}
