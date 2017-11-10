package fi.kapsi.kosmik.sfti.ch12

/**
  * Implement the factorial function using to and reduceLeft , without a loop or
  * recursion.
  */
object Ex03 {
  def factReduce(n: Int): Int = {
    if (n > 0) 1 to n reduceLeft (_ * _)
    else if (n == 0) 1
    else throw new IllegalArgumentException
  }
}
