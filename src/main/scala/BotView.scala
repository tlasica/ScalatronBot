/**
 * Created by tomek on 13.01.15.
 */
case class BotView(str: String) {

  // Master Bot view is 31x31, mini-bot view is 21x21
  require(str.length==BotView.MasterViewNumCells || str.length==BotView.MiniViewNumCells)

  def size = if (str.length == 31*31) 31 else 21

  // 0..30 x 0.30, (0,0) upper left corner
  def at(row: Int, col: Int) = {
    require(row>=0 && row<size)
    require(col>=0 && col<size)
    val idx = row * size + col
    str(idx)
  }

  override def toString(): String = str
}

object BotView {
  val Hidden = '?'
  val Empty = '-'
  val Wall = 'W'
  val Master = 'M'
  val EnemyMaster = 'm'
  val Mini = 'S'
  val EnemyMini = 's'
  val Zugar = 'P'
  val Toxifera = 'p'
  val Fluppet = 'B'
  val Snorg = 'b'

  val MasterPos = Coord(15,15)
  val MiniPos = Coord(10,10)

  val MasterviewSize = 31
  val MiniViewSize = 21

  val MasterViewNumCells = MasterviewSize * MasterviewSize
  val MiniViewNumCells = MiniViewSize * MiniViewSize

}

// TODO: method at(x,y) may be replaced with apply
// TODO: what to do with invalid views