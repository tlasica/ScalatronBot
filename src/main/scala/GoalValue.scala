object GoalValue {

  // 0..100
  def distanceValue(d: Int): Long = {
    val Exp = 5.0
    val max = math.pow(31.0, Exp)
    val act = math.pow(31.0 - d, Exp)
    math.round( 1000.0 * act / max )
  }


  private[this] def cellValue(cell: Char, dist: Int) : Long = {
    cell match {
      case BotView.Wall => wallValue(dist)
      case BotView.Master => 0
      case BotView.EnemyMaster => -100 * distanceValue(dist)
      case BotView.EnemyMini => -300 * distanceValue(dist)
      case BotView.Zugar => if (dist==0) 200000 else 150 * distanceValue(dist)
      case BotView.Toxifera => if (dist>0) 0 else -50000
      case BotView.Fluppet => fluppetValue(dist)
      case BotView.Snorg => snorgValue(dist)
      case BotView.Empty => 4
      case _ => 0
    }
  }

  private[this] def wallValue(dist: Int): Long =  dist match {
    case 0  => -300
    case -1 => -1
    case _  => 0
  }

  private[this] def snorgValue(dist: Int): Long = {
    if (dist==0) -300000L else -250 * distanceValue(dist)
  }

  private[this] def fluppetValue(dist: Int): Long = {
    if (dist==0) 500000L else 200 * distanceValue(dist)
  }

  def forView(view: BotView, pos: Coord): Long = {
    val distanceXY = Distance.calculateMap(view, pos)
    val distanceVector = distanceXY.flatten
    val viewWithDistance = view.toString().toCharArray zip distanceVector
    val productVector = viewWithDistance map { case(what:Char, dist:Int) => cellValue(what, dist) }
    productVector.sum
  }

  def situation(view: BotView, row: Int, col: Int): List[String] = {
    def observationString(r: Int, c:Int, cell: Char, dist: Int): String = {
      if (BotView.isInteresting(cell)) "%s(%d,%d)~%d".format(cell,r,c,dist)
      else ""
    }

    val distance = Distance.calculateMap(view, Coord(row, col))
    val sit = for {
      r <- Range(0, 31)
      c <- Range(0, 31)
      cell = view.at(r, c)
      dist = distance(r)(c)
    } yield observationString(r, c, cell, dist)
    sit filter (_.nonEmpty) toList
  }


}
