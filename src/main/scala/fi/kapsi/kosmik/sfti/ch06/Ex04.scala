package fi.kapsi.kosmik.sfti.ch06

/**
  * Define a Point class with a companion object so that you can construct Point
  * instances as Point(3, 4), without using new.
  */
class Point(val x: Int, val y: Int)

object Point {
  def apply(x: Int, y: Int): Point = new Point(x, y)
}

object Example {
  val point: Point = Point(1, 2)
}
