package fi.kapsi.kosmik.sfti.ch12

/**
  * In Section 12.8, “Currying,” on page 164, you saw the corresponds method used
  * with two arrays of strings. Make a call to corresponds that checks whether the
  * elements in an array of strings have the lengths given in an array of integers.
  */
object Ex08 {
  def correspondingLengths(s: Array[String], i: Array[Int]): Boolean = s.corresponds(i)(_.length == _)
}
