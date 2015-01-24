/**
 * Created by tomek on 17.01.15.
 */
object GoalValue {

  // 0..100
  private def distanceValue(d: Int): Long = {
    scala.math.round(100.0 * ((31-d)*(31-d) / (31.0*31.0) ) )
  }


  //  map ((x:Int) => BotView.MasterviewSize-x)
  private def cellValue(cell: Char, dist: Int) : Long = {
    cell match {
//      case BotView.Wall =>
//        if (dist == 0) -1000
//        if (dist < 4) -33
//        else 0
      case BotView.Master => 0
      case BotView.EnemyMaster => -100 * distanceValue(dist)
      case BotView.EnemyMini => -300 * distanceValue(dist)
      case BotView.Zugar => if (dist==0) 20000 else 150 * distanceValue(dist)
      case BotView.Toxifera => if (dist>0) 0 else -100 * distanceValue(dist)
      case BotView.Fluppet => if (dist==0) 50000 else 200 * distanceValue(dist)
      case BotView.Snorg => if (dist==0) -30000 else -150 * distanceValue(dist)
      //case BotView.Empty => 1
      case _ => 0
    }
  }


  private def groundFromView(view: BotView): Array[Array[Boolean]] = {
    val groundLine = view.toString().toCharArray map ( (x:Char) => if (x=='W') false else true )
    Array.tabulate(view.size, view.size)( (x,y) => groundLine(x*view.size + y) )
  }

  def forView(view: BotView, pos: Coord): Long = {
    val ground = groundFromView(view)
    val distanceXY = Distance.calculateDistanceArray(ground, pos.row, pos.col)
    val distanceVector = distanceXY.flatten
    val viewWithDistance = view.toString().toCharArray zip distanceVector
    //println(viewWithDistance filter ((x:(Char,Int)) => (x._1 != 'W') && (x._1!='_' && (x._1!='?'))) mkString)
    val productVector = viewWithDistance map ( (x:(Char,Int)) => cellValue(x._1, x._2) )
    //println(productVector.filter(_!=0).mkString(", "))
    productVector.sum
  }

  def situation(view: BotView, row: Int, col: Int): List[String] = {
    def observationString(r: Int, c:Int, cell: Char, dist: Int): String = {
      val what = cell match {
        case 'm' => "Enemy Master(m)"
        case 's' => "Enemy Mini(s)"
        case 'P' => "Zugar(P)"
        case 'p' => "Toxifera(p)"
        case 'B' => "Fluppet(B)"
        case 'b' => "Snorg(b)"
        case _ => ""
      }
      if (what.nonEmpty)
        cell +"(" + r + "," + c + ")~" + dist
      else ""
    }

    val ground = groundFromView(view)
    val distance = Distance.calculateDistanceArray(ground, row, col)
    val sit = for {
      r <- Range(0, 31)
      c <- Range(0, 31)
      cell = view.at(r, c)
      dist = distance(r)(c)
    } yield observationString(r, c, cell, dist)
    sit filter (_.nonEmpty) toList
  }


}
