object Distance {

  def directions: List[Coord] = {
    for{
      dr <-List(-1, 0, 1)
      dc <-List(-1, 0, 1)
      if dr!=0 || dc!=0
    } yield Coord(dr, dc)
  }

  def neighbours(row: Int, col: Int, size:Int): List[(Int, Int)] = {
    for {
      dir <- directions
      nr = row + dir.row
      nc = col + dir.col
      if nr >= 0 && nr < size
      if nc >= 0 && nc < size
    } yield(nr, nc)
  }

  def calculateDistanceArray(view: BotView, center:Coord): Array[Array[Int]] = {
    val ground = groundFromView(view)
    calculateDistanceArray(ground, center)
  }

  /**
   * Assumption: wall/not accesible == -1, free = Int.MaxValue
   * @param ground
   * @param start
   */
  def calculateDistanceArray(ground: Array[Array[Boolean]], center:Coord): Array[Array[Int]] = {
    val size = ground.length
    val distanceMap = Array.fill(size, size)(Int.MaxValue)
    def isfree(row: Int, col:Int): Boolean = ground(row)(col)

    def visit(pos: Coord): List[(Coord, Int)] = {
      val dist = distanceMap(pos.row)(pos.col)
      val updates = for{
        (nRow,nCol) <- neighbours(pos.row, pos.col, size)
        if isfree(nRow, nCol)
        if dist + 1 < distanceMap(nRow)(nCol)
      } yield (Coord(nRow, nCol), dist + 1)
      updates
    }

    import scala.collection.mutable.Queue
    val waiting = new Queue[Coord]()

    // push start point to start visiting nodes
    distanceMap(center.row)(center.col) = 0
    waiting.enqueue( center )

    // BFS over the ground
    while( waiting.nonEmpty) {
      val next = waiting.dequeue()
      val updates = visit(next)
      for(u <- updates) {
        val (coord, d) = u
        distanceMap(coord.row)(coord.col) = d
        waiting.enqueue(coord)
      }
    }

    // calculate distance for walls as min() wall neighbours
    for(row <- Range(0, size))
      for(col <- Range(0, size)) {
        if ( ! isfree(row, col) ) {
          val nearby = neighbours(row, col, size)
          val d = nearby map ( (x: (Int, Int)) => distanceMap(x._1)(x._2) )
          distanceMap(row)(col) = 1 + d.min
        }
      }

    distanceMap
  }


  def directionsWithVisibility(view: BotView, pos: Coord): List[(Coord, Int)] = {
    val directions = Distance.directions
    directions map (d => (d, view.visibility(pos, d) ) )
  }

  def mostVisibleDirection(view: BotView, pos: Coord): (Coord, Int) = {
    val all = directionsWithVisibility(view, pos)
    val bestVis = all map ( (x:(Coord,Int)) => x._2) max
    val bestDirs = all filter ( (x:(Coord,Int)) => x._2 == bestVis)
    val best = util.Random.shuffle(bestDirs).head
    best
  }


  private def groundFromView(view: BotView): Array[Array[Boolean]] = {
    val groundLine = view.toString().toCharArray map ( (x:Char) => if (x==BotView.Wall) false else true )
    Array.tabulate(view.size, view.size)( (x,y) => groundLine(x*view.size + y) )
  }

}
