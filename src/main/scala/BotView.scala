case class ViewFact(what: Char, coord: Coord, distance: Int)

case class BotView(str: String) {

  private val MasterView = 31
  private val MiniView = 21

  // Master Bot view is 31x31, mini-bot view is 21x21
  require(str.length == BotView.MasterViewSize * BotView.MasterViewSize ||
          str.length == BotView.MiniViewSize * BotView.MiniViewSize)

  val size = BotView.viewSize(str)

  // 0..30 x 0.30, (0,0) upper left corner
  def at(row: Int, col: Int): Char = {
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
    res.toList
  }

  def walls(from: Coord): List[ViewFact] = {
    val distanceMap = Distance.calculateDistanceArray(this, from)
    val interestingWithIndex = str.zipWithIndex filter ( (x:(Char,Int)) => x._1==BotView.Wall)
    val res = for{
      (what, idx) <- interestingWithIndex
      coord = toCoord(idx)
      dist = distanceMap(coord.row)(coord.col)
    } yield ViewFact(what, coord, dist)
    res.toList

  }


  def visibility(from: Coord, dir: Coord): Int = {
    import BotView.isSafe
    var newPos = from.add(dir)
    var vis = 0
    while (isPositionCorrect(newPos) && isSafe(at(newPos)) ) {
      vis += 1
      newPos = newPos.add(dir)
    }
    vis
  }

  override def toString(): String = str

  private def toCoord(idx: Int) = Coord(idx / size, idx % size)
}


object BotViewPrinter {

  def square(viewStr: String): String = {
    val size = BotView.viewSize(viewStr)
    val withIndex = viewStr.zipWithIndex
    val rows = withIndex map ( ci => if ((ci._2 + 1) % size == 0) ci._1 + "\n" else ci._1 )
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

  val MasterViewSize = 31
  val MiniViewSize = 21

  //lazy val MasterViewNumCells = MasterviewSize * MasterviewSize
  //lazy val MiniViewNumCells = MiniViewSize * MiniViewSize

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

  def viewSize(str: String): Int = {
    if (str.length == MasterViewSize * MasterViewSize) MasterViewSize else MiniViewSize
  }

}

object MasterPosition {
  val row = 15
  val col = 15
  val coord = Coord(row, col)
}

object MiniPosition {
  val row = 10
  val col = 10
  val coord = Coord(row, col)
}
