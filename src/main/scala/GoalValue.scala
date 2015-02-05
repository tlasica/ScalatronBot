object GoalValue {

  // 0..100
  def distanceValue(d: Int): Long = {
    val Exp = 5.0
    val max = math.pow(31.0, Exp)
    val act = math.pow(31.0 - d, Exp)
    math.round( 1000.0 * act / max )
  }


  //  map ((x:Int) => BotView.MasterviewSize-x)
  private def cellValue(cell: Char, dist: Int) : Long = {
    def wallValue: Int = {
      if (dist == 0) -300
      else if (dist == -1) -1
      else 0
    }

    cell match {
      case BotView.Wall => wallValue
      case BotView.Master => 0
      case BotView.EnemyMaster => -100 * distanceValue(dist)
      case BotView.EnemyMini => -300 * distanceValue(dist)
      case BotView.Zugar => if (dist==0) 200000 else 150 * distanceValue(dist)
      case BotView.Toxifera => if (dist>0) 0 else -50000
      case BotView.Fluppet => if (dist==0) 500000 else 200 * distanceValue(dist)
      case BotView.Snorg => if (dist==0) -300000 else -250 * distanceValue(dist)
      case BotView.Empty => 4
      case _ => 0
    }
  }


  def forView(view: BotView, pos: Coord): Long = {
    val distanceXY = Distance.calculateDistanceArray(view, pos)
    val distanceVector = distanceXY.flatten
    val viewWithDistance = view.toString().toCharArray zip distanceVector
    val productVector = viewWithDistance map ( (x:(Char,Int)) => cellValue(x._1, x._2) )
    productVector.sum
  }

  def situation(view: BotView, row: Int, col: Int): List[String] = {
    def observationString(r: Int, c:Int, cell: Char, dist: Int): String = {
      if (BotView.isInteresting(cell)) "%s(%d,%d)~%d".format(cell,r,c,dist)
      else ""
    }

    val distance = Distance.calculateDistanceArray(view, Coord(row, col))
    val sit = for {
      r <- Range(0, 31)
      c <- Range(0, 31)
      cell = view.at(r, c)
      dist = distance(r)(c)
    } yield observationString(r, c, cell, dist)
    sit filter (_.nonEmpty) toList
  }


}
