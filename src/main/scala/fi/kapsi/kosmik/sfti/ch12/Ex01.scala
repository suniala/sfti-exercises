package fi.kapsi.kosmik.sfti.ch12

/**
  * Write a function values(fun: (Int) => Int, low: Int, high: Int) that yields a collec-
  * tion of function inputs and outputs in a given range. For example, values(x =>
  * x * x, -5, 5) should produce a collection of pairs (-5, 25) , (-4, 16) , (-3, 9) , . . . ,
  * (5, 25) .
  */
object Ex01 {
  def values(fun: (Int) => Int, low: Int, high: Int): List[(Int, Int)] = {
    (low to high map (i => (i, fun(i)))).toList
  }
}
