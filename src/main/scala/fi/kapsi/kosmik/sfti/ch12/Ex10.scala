package fi.kapsi.kosmik.sfti.ch12

/**
  * Implement an unless control abstraction that works just like if , but with an
  * inverted condition. Does the first parameter need to be a call-by-name
  * parameter? Do you need currying?
  */
object Ex10 {
  def callByValueUnless(condition: () => Boolean)(block: => Unit) {
    if (!condition()) {
      block
    }
  }

  def callByNameUnless(condition: => Boolean)(block: => Unit) {
    if (!condition) {
      block
    }
  }

  def nonCurryingUnless(condition: => Boolean, block: => Unit) {
    if (!condition) {
      block
    }
  }
}
