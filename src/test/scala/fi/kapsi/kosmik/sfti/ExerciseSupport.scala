package fi.kapsi.kosmik.sfti

import java.time.Duration

import org.scalatest.{Informing, Matchers}

trait ExerciseSupport extends Matchers with Informing {
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

  /**
    * Evaluate given thunk and assert its evaluation takes an expected amount of time.
    *
    * @param expected evaluation should take less time than this
    * @param thunk    block of code to evaluate
    * @tparam T thunk result type
    * @return result of thunk evaluation
    */
  def assertDurationLess[T](expected: Duration)(thunk: => T): T = {
    val t1 = System.nanoTime()
    val result = thunk
    val t2 = System.nanoTime()

    val duration = Duration.ofNanos(t2 - t1)
    if (duration.compareTo(expected) > 0) {
      fail(f"took ${format(duration)}, expected to take less than ${format(expected)}")
    } else {
      succeed
    }

    result
  }

  def format(d: Duration): String = f"${d.toNanos.toDouble / 1000} microseconds"

}
