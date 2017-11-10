package fi.kapsi.kosmik.sfti.ch12

/**
  * It’s easy to get a sequence of pairs, for example:
  * <pre>
  * val pairs = (1 to 10) zip (11 to 20)
  * </pre>
  * Now, suppose you want to do something with such a sequence—say, add
  * up the values. But you can’t do
  * <pre>
  * pairs.map(_ + _)
  * </pre>
  * The function _ + _ takes two Int parameters, not an (Int, Int) pair. Write a
  * function adjustToPair that receives a function of type (Int, Int) => Int and returns
  * the equivalent function that operates on a pair. For example, adjustToPair(_ *
  * _)((6, 7)) is 42 .
  * Then use this function in conjunction with map to compute the sums of the
  * elements in pairs .
  */
object Ex07 {
  def adjustToPair(fun: (Int, Int) => Int): ((Int, Int)) => Int = {
    case (a, b) => fun(a, b)
  }
}
