/**
 * Created by tomek on 25.01.15.
 */
object GuardianMiniBot {

  def react(reactCmd: ReactCmd): List[BotCommand] = {
    var res: List[BotCommand] = List()
    if (reactCmd.view.isDefined) {
      val view = reactCmd.view.get
      val facts = view.factsWithDistance(MiniPosition.coord)
      val snorgs = facts filter (_.what == BotView.Snorg)
      val closeSnorgs = snorgs filter (_.distance <= 3)

      if (closeSnorgs.nonEmpty) {
        val dist = snorgs map ( (x:ViewFact) => x.distance)
        res = List(ExplodeCommand(3))
      }
    }
    res
  }

}
