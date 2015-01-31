/**
 * Created by tomek on 31.01.15.
 */
object HarvesterMiniBot extends Bot {

  def forceReturn(energy: Int) = energy > 2000
  def worthReturn(energy: Int) = energy > 1000

  private def statusString(cmd:ReactCmd) = "%s[%d]".format(cmd.name, cmd.energy)

  override def react(reactCmd: ReactCmd): List[BotCommand] = {
    require(reactCmd.masterPosition.isDefined)
    val status = statusString(reactCmd)
    val facts = reactCmd.view.factsWithDistance(MiniPosition.coord)
    val moveForGoods = moveToNearestGood(reactCmd, facts)
    val moveEscapeSnorgs = escapeSnorgs(reactCmd, facts)
    val moveReturn = returnToMaster(reactCmd, facts)
    val random = randoMove(reactCmd, facts)
    if (forceReturn(reactCmd.energy) && moveReturn.isDefined) moveReturn.get :: List( StatusCommand(status+"[fr]") )
    if (moveForGoods.isDefined) moveForGoods.get  :: List( StatusCommand(status+"[h]") )
    else if (moveEscapeSnorgs.isDefined) moveEscapeSnorgs.get :: List( StatusCommand(status+"[s]") )
    else if (moveReturn.isDefined) moveReturn.get :: List( StatusCommand(status+"[r]") )
    else if (random.isDefined) random.get :: List( StatusCommand(status+"[!]") )
    else List( StatusCommand(status+"[?]") )
  }

  private def returnToMaster(cmd: ReactCmd, facts: List[ViewFact]): Option[BotCommand] = {
    if ( worthReturn(cmd.energy) ) {
      val master = cmd.masterPosition.get
      val move = Coord(Math.signum(master.col).toInt, Math.signum(master.row).toInt)
      val similar = move.similarDirections
      val available = (move :: similar) filter ( (x:Coord) => BotView.isSafe( cmd.view.at( MiniPosition.coord.add(x) ) ) )
      if (available.nonEmpty) Some(MoveCommand(available.head)) else None
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

}
