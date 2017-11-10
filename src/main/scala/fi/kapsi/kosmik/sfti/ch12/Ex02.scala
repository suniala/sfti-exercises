package fi.kapsi.kosmik.sfti.ch12

/**
  * How do you get the largest element of an array with reduceLeft ?
  */
object Ex02 {
  def largest(arr: Array[Int]): Int = arr.reduceLeft((a, b) => if (a > b) a else b)
}
