/**
 * Created by tomek on 25.01.15.
 */
object GuardianMiniBot extends Bot {

  def react(reactCmd: ReactCmd): List[BotCommand] = {
    val view = reactCmd.view
    val facts = view.factsWithDistance(MiniPosition.coord)
    val allSnorgs = facts filter (_.what == BotView.Snorg)
    val nearSnorgs = allSnorgs filter (_.distance <= 3)

    if (nearSnorgs.nonEmpty) {
      val range = 3
      List(ExplodeCommand(range))
    }
    else List(StatusCommand(reactCmd.name))
  }

}
