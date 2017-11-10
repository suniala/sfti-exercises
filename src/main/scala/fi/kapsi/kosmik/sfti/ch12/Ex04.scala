package fi.kapsi.kosmik.sfti.ch12

/**
  * The previous implementation needed a special case when n < 1. Show how
  * you can avoid this with foldLeft . (Look at the Scaladoc for foldLeft . It’s like
  * reduceLeft , except that the first value in the chain of combined values is supplied
  * in the call.)
  */
object Ex04 {
  /**
    * Implementation note: Parentheses are required here. Otherwise the compiler will think we are trying to call
    * Int(1) with parameters.
    */
  def factFold(n: Int): Int = (1 to n foldLeft 1) (_ * _)
}
