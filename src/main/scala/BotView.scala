/**
 * Created by tomek on 13.01.15.
 */
case class BotView(str: String) {

  // Master Bot view is 31x31, mini-bot view is 21x21
  require(str.length==31*31 || str.length==21*21)

  private def size = if (str.length == 31*31) 15 else 10
  private def rowSize = if (str.length == 31*31) 31 else 21

  // -15..15 or -10..10
  def at(x: Int, y: Int) = {
    val row = y + 15
    val col = x + 15
    val idx = row * rowSize + col
    if (idx >= 961) {
      println ("x:" + x + " y:" + y)
    }
    str(idx)
  }
}

object BotView {
  val Hidden = '?'
  val Empty = '-'
  val Wall = 'W'
  val Master = 'M'
  val EnemyMaster = 'm'
  val Zugar = 'P'
  val Toxifera = 'p'
  val Flupper = 'B'
  val Snorg = 'b'

}

// TODO: method at(x,y) may be replaced with apply
// TODO: what to do with invalid views