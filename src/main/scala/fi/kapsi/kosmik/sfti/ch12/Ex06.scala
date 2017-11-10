package fi.kapsi.kosmik.sfti.ch12

/**
  * Modify the previous function to return the input at which the output is largest.
  * For example, largestAt(x => 10 * x - x * x, 1 to 10) should return 5 . Don’t use
  * a loop or recursion.
  **/
object Ex06 {
  def largestAt(fun: (Int) => Int, inputs: Seq[Int]): Int =
    inputs
      .map(n => (n, fun(n)))
      .foldLeft((Int.MinValue, Int.MinValue))({ case ((ai, ao), (bi, bo)) => if (ao > bo) (ai, ao) else (bi, bo) })
      ._1
}

