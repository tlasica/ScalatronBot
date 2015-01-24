/**
 * Created by tomek on 25.01.15.
 */
object GuardianMiniBot {

  def react(reactCmd: ReactCmd): List[BotCommand] = {
    if (reactCmd.view.isDefined) {
      val view = reactCmd.view.get
      val snorgs = view.find(BotView.Snorg)


      List(ExplodeCommand(3))
    }
    else List()
  }

}
