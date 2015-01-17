/**
 * Created by tomek on 15.01.15.
 * Baaaardzo nie funkcyjne
 */

object Distance {

  /* Dlaczego to się nie chce skompilować??
  def xyFromLineArray[T](in: Array[T], rowSize: Int): Array[Array[T]] = {
    Array.tabulate(rowSize, rowSize)[T]( (x,y) => in(x*rowSize + y) )
  }
  */

  def neighbours(row: Int, col: Int, size:Int): List[(Int, Int)] = {
    for {
      dr <-List(-1, 0, 1)
      dc <-List(-1, 0, 1)
      nr = row + dr
      nc = col + dc
      if nr >= 0 && nr < size
      if nc >= 0 && nc < size
    } yield(nr, nc)
  }

  /**
   * Assumption: wall/not accesible == -1, free = Int.MaxValue
   * @param ground
   * @param start
   */
  def calculateDistanceArray(ground: Array[Array[Boolean]], mRow: Int, mCol:Int): Array[Array[Int]] = {
    val size = ground.length
    val distanceMap = Array.fill(size, size)(Int.MaxValue)
    def isfree(row: Int, col:Int): Boolean = ground(row)(col)

    def visit(row: Int, col:Int): List[ (Int, Int, Int)] = {
      val dist = distanceMap(row)(col)
      val updates = for{
        (nRow,nCol) <- neighbours(row, col, size)
        if isfree(nRow, nCol)
        if dist+1 < distanceMap(nRow)(nCol)
      } yield (nRow, nCol, dist+1)
      updates
    }

    import scala.collection.mutable.Queue
    val waiting = new Queue[(Int, Int)]()

    // push start point to start visiting nodes
    distanceMap(mRow)(mCol) = 0
    waiting.enqueue( (mRow, mCol) )

    while( waiting.nonEmpty) {
      val next = waiting.dequeue()
      val updates = visit(next._1, next._2)
      for(u <- updates) {
        val (r, c, d) = u
        distanceMap(r)(c) = d
        waiting.enqueue((r, c))
      }
    }
    distanceMap

  }

}