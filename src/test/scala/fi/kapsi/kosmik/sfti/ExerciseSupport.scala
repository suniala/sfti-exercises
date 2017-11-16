package fi.kapsi.kosmik.sfti

import org.scalatest.FunSpecLike

/**
  * Implementation note: currently this extends on FunSpecLike as said trait provides a "info" method and all test
  * cases happen to use said trait.
  */
trait ExerciseSupport extends FunSpecLike {
  /**
    * Evaluate given thunk and time its evaluation.
    *
    * @param label attach this label to info messages
    * @param thunk block of code to evaluate
    * @tparam T thunk result type
    * @return result of thunk evaluation
    */
  def evaluateAndTime[T](label: String)(thunk: => T): T = {
    info(f"timing '$label'")
    val t1 = System.nanoTime()
    val result = thunk
    val t2 = System.nanoTime()
    info(f"  -> '$label' took ${(t2 - t1).toDouble / 1000} microseconds")
    result
  }
}
