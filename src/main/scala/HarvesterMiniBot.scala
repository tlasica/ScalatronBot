import scala.collection.mutable

/**
 * Created by tomek on 31.01.15.
 */
class HarvesterMiniBot extends Bot {

  private def forceReturn(energy: Int) = energy > 2000
  private def worthReturn(energy: Int) = energy > 1000

  private def statusString(cmd:ReactCmd) = "%s[%d]".format(cmd.name, cmd.energy)

  private var escapeDir: Option[Coord] = None
  private var escapeSteps: Int = 0

  private def escapeMove(reactCmd: ReactCmd): Option[BotCommand] = {
    if (escapeDir.isDefined) {
      val dest = MiniPosition.coord add escapeDir.get
      if ( escapeSteps>0 && reactCmd.view.at(dest) == BotView.Empty ) {
        escapeSteps = escapeSteps - 1
        Some(MoveCommand(escapeDir.get))
      }
      else {
        escapeDir = None
        escapeSteps = 0
        None
      }
    }
    else None
  }

  override def react(reactCmd: ReactCmd): List[BotCommand] = {
    val status = statusString(reactCmd)

    val escape = escapeMove(reactCmd)

    if (escape.isDefined) {
      List(escape.get, StatusCommand(status+"[e]"))
    }
    else {
      val facts = reactCmd.view.factsWithDistance(MiniPosition.coord)
      val moveForGoods = moveToNearestGood(reactCmd, facts)
      val moveEscapeSnorgs = escapeSnorgs(reactCmd, facts)
      val moveReturn = returnToMaster(reactCmd, facts)
      //val random = randoMove(reactCmd, facts)
      if (forceReturn(reactCmd.energy) && moveReturn.isDefined) moveReturn.get :: List( StatusCommand(status+"[fr]") )
      if (moveForGoods.isDefined) moveForGoods.get :: List( StatusCommand(status+"[h]") )
      else if (moveEscapeSnorgs.isDefined) moveEscapeSnorgs.get :: List( StatusCommand(status+"[s]") )
      else if (moveReturn.isDefined) moveReturn.get :: List( StatusCommand(status+"[r]") )
      else {
        escapeDir = Some(prepareEscape(reactCmd.view))
        escapeSteps = 17
        List()
      }
    }
  }

  private def returnToMaster(cmd: ReactCmd, facts: List[ViewFact]): Option[BotCommand] = {
    val master = cmd.masterPosition.get
    val masterDist = Math.round(Math.sqrt(master.row*master.row + master.col*master.col))
    if ( worthReturn(cmd.energy) ) {
      val move = Coord(Math.signum(master.col).toInt, Math.signum(master.row).toInt)
      val similar = move.similarDirections
      val available = (move :: similar) filter ( (x:Coord) => BotView.isSafe( cmd.view.at( MiniPosition.coord.add(x) ) ) )
      val visibleDirs = for {
        dir <- available
        v = cmd.view.visibility(MiniPosition.coord, dir)
        if v > masterDist / 2.0
      } yield dir
      if (visibleDirs.nonEmpty) Some(MoveCommand(visibleDirs.head)) else None
    }
    else None
  }


  private def randoMove(cmd: ReactCmd, facts: List[ViewFact]): Option[BotCommand] = {
    val safeDirections = for{
      d <- Distance.directions
      n = MiniPosition.coord.add(d)
      if BotView.isSafe(cmd.view.at(n))
    } yield d
    if (safeDirections.nonEmpty) Some(MoveCommand(util.Random.shuffle(safeDirections).head)) else None
  }


  private def moveToNearestGood(cmd: ReactCmd, facts:List[ViewFact]): Option[BotCommand] = {
    val eatable = facts filter ((f:ViewFact) => f.what == BotView.Zugar || f.what == BotView.Fluppet )
    if (eatable.nonEmpty) {
      val best = eatable.sortWith( _.distance < _.distance ).head
      val dest = best.coord
      val move = MiniPosition.coord.findDirTo(dest)
      val similar = move.similarDirections
      val available = (move :: similar) filter ( (x:Coord) => BotView.isSafe( cmd.view.at( MiniPosition.coord.add(x) ) ) )
      if ( available.nonEmpty ) Some(MoveCommand(available.head)) else None
    }
    else None
  }

  private def escapeSnorgs(cmd: ReactCmd, facts: List[ViewFact]): Option[BotCommand] = {
    val nearSnorgs = facts filter ((f:ViewFact) => f.what == BotView.Snorg && f.distance<3 )
    if (nearSnorgs.nonEmpty) {
      val approachingFrom = nearSnorgs map ( (f:ViewFact) => MiniPosition.coord.findDirTo(f.coord) )
      val safeDirections = for{
        d <- Distance.directions
        a = approachingFrom filter (_ == d)
        if a.isEmpty
        n = MiniPosition.coord.add(d)
        if BotView.isSafe(cmd.view.at(n))
      } yield d
      if (safeDirections.nonEmpty) Some(MoveCommand(safeDirections.head)) else Some(ExplodeCommand(3))
    }
    else None
  }

  private def prepareEscape(view: BotView): Coord = {
    val (bestDir, bestVis) = Distance.mostVisibleDirection(view, MiniPosition.coord)
    bestDir
  }

  private def escapeFromWalls(view: BotView): Option[BotCommand] = {
    val options = for{
      dir <- Distance.directions
      dest = MiniPosition.coord.add(dir)
      if BotView.isSafe(view.at(dest))
      wallsWeight = view.walls(dest) map ((x:ViewFact) => Math.round(100.0*(21.0-x.distance)/21.0)) sum
    } yield (dir, wallsWeight)
    if (options.nonEmpty) {
      println("escape from walls options: " + options.mkString(";"))
      val minWeight = options map ((x:(Coord, Long)) => x._2) min
      val bestDirs = options filter ( (x:(Coord, Long)) => x._2 == minWeight)
      Some(MoveCommand(bestDirs.head._1))
    }
    else None
  }

}
