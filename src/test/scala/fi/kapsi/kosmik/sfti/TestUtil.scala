package fi.kapsi.kosmik.sfti

object TestUtil {
  def evaluateAndTime[T](log: String => Unit, label: String)(thunk: => T): T = {
    log("timing \"" + label + "\"")
    val t1 = System.nanoTime()
    val result = thunk
    val t2 = System.nanoTime()
    log("  -> took " + (t2 - t1).toDouble / 1000 + " milliseconds")
    result
  }
}
