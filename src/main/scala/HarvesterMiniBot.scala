/**
 * Created by tomek on 31.01.15.
 */
object HarvesterMiniBot extends Bot {

  val EnergyReturnLevel = 1000

  override def react(reactCmd: ReactCmd): List[BotCommand] = {
    require(reactCmd.masterPosition.isDefined)
    val res = List(StatusCommand(reactCmd.name))
    if (reactCmd.energy >= EnergyReturnLevel) {
      MoveCommand(reactCmd.masterPosition.get)::res
    }
    else {
      moveToNearestGood(reactCmd)::res
    }
  }

  private def moveToNearestGood(cmd: ReactCmd): BotCommand = {
    val facts = cmd.view.factsWithDistance(MiniPosition.coord)
    val neighbours = Distance.neighbours(MiniPosition.row, MiniPosition.col, 21)
    println(neighbours)
    val availableNeighbours = neighbours filter ( (x:(Int, Int)) => cmd.view.at(x._1, x._2) != BotView.Wall)
    val moves = for {
      n <- availableNeighbours
      newPos:Coord = Coord(n._1, n._2)
      move = MiniPosition.coord.findMoveTo(newPos)
      value = GoalValue.forView( cmd.view, newPos )
    } yield (move, value)
    val best = moves.sortWith( _._2 > _._2).head
    println(moves.mkString(","))
    MoveCommand(best._1)
  }
}
