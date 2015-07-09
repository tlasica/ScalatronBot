/**
 * Created by tomek on 24.01.15.
 */
class BotViewBuilder(size: Int) {

  val view = Array.fill(size * size)(BotView.Empty)

  def create: BotView = {
    BotView( new String(view) )
  }

  def createString: String = {
    new String(view)
  }

  def add(what: Char, row: Int, col: Int) = {
    val idx = row * size + col
    view(idx) = what
  }

  def addRow(what: Char, row: Int) = {
    Range(0,size) foreach {add(what, row, _)}
  }

  def addCol(what: Char, col: Int) =  {
    Range(0,size) foreach {add(what, _, col)}
  }

}
