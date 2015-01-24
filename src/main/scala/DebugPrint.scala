trait DebugPrint {
  def debug(in: String)
  def clearDebug()
  def printDebug()
}

trait SimpleDebugPrint extends DebugPrint {
  val d: StringBuilder = new StringBuilder
  override def debug(in: String) { d ++= in + "\n" }
  override def clearDebug() { d.clear() }
  override def printDebug() { println( d.toString() ) }
}

trait NoDebugPrint extends DebugPrint {
  override def debug(in: String) {/* noop */}
  override def clearDebug() {/* noop */}
  override def printDebug() {/* noop */}
}