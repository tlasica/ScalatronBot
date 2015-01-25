/**
 * Created by tomek on 25.01.15.
 */
object GuardianMiniBot {

  def react(reactCmd: ReactCmd): List[BotCommand] = {
    var res: List[BotCommand] = List()
    if (reactCmd.view.isDefined) {
      val view = reactCmd.view.get
      val snorgs: List[Coord] = view.find(BotView.Snorg)
      if (snorgs.nonEmpty) {
        val distanceMap = Distance.calculateDistanceArray(view, MiniPosition.row, MiniPosition.col)
        val snorgsDistances = snorgs.map( (x: Coord) => distanceMap(x.row)(x.col))
        val minDist = snorgsDistances min
        val maxDist = snorgsDistances max

        if (minDist <= 3) {
          res = List(ExplodeCommand(4 min maxDist))
        }
      }
    }
    res
  }

}
