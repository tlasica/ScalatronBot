case class ViewFact(what: Char, coord: Coord, distance: Int)

case class BotView(str: String) {

  //TODO: eliminate one of the at() methods and use Coords in all functions

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

  def at(coord: Coord): Char = at(coord.row, coord.col)

  def isPositionCorrect(coord: Coord): Boolean = {
    (coord.row>=0 && coord.row<size) && (coord.col>=0 && coord.col<size)
  }

  def find(what: Char): List[Coord] = {
    val filteredWithIndex = str.zipWithIndex filter (_._1 == what)
    filteredWithIndex map (x => toCoord(x._2)) toList
  }

  def factsWithDistance(from: Coord): List[ViewFact] = {
    val distanceMap = Distance.calculateDistanceArray(this, from)
    val interestingWithIndex = str.zipWithIndex filter ( (x:(Char,Int)) => BotView.isInteresting(x._1))
    val res = for{
      (what, idx) <- interestingWithIndex
      coord = toCoord(idx)
      dist = distanceMap(coord.row)(coord.col)
    } yield ViewFact(what, coord, dist)
    res toList
  }

  override def toString(): String = str

  private def toCoord(idx: Int) = Coord(idx / size, idx % size)
}


object BotViewPrinter {

  def square(viewStr: String): String = {
    val size = if (viewStr.length == 31*31) 31 else 21
    val withIndex = viewStr.zipWithIndex
    val rows = withIndex map ( ci => if ((ci._2+1) % size == 0) ci._1 + "\n" else ci._1 )
    rows.mkString
  }

}



object BotView {
  val Hidden = '?'
  val Empty = '_'
  val Wall = 'W'
  val Master = 'M'
  val EnemyMaster = 'm'
  val Mini = 'S'
  val EnemyMini = 's'
  val Zugar = 'P'
  val Toxifera = 'p'
  val Fluppet = 'B'
  val Snorg = 'b'

  val MasterviewSize = 31
  val MiniViewSize = 21

  val MasterViewNumCells = MasterviewSize * MasterviewSize
  val MiniViewNumCells = MiniViewSize * MiniViewSize

  def isInteresting(what: Char): Boolean = what match {
    case Hidden => false
    case Empty => false
    case Wall => false
    case _ => true
  }

  def isSafe(cell: Char): Boolean = {
    cell match {
      case BotView.Wall => false
      case BotView.EnemyMaster => false
      case BotView.Toxifera => false
      case BotView.Snorg => false
      case _ => true
    }
  }

}

object MasterPosition {
  def row = 15
  def col = 15
  def coord = Coord(15, 15)
}

object MiniPosition {
  def row = 10
  def col = 10
  def coord = Coord(10, 10)
}

// TODO: method at(x,y) may be replaced with apply
// TODO: what to do with invalid views