package fi.kapsi.kosmik.sfti.ch10

import java.awt.Point

/**
  * Define a class OrderedPoint by mixing scala.math.Ordered[Point] into java.awt.Point .
  * Use lexicographic ordering, i.e. (x, y) < (x’, y’) if x < x’ or x = x’ and y < y’.
  */
object Ex02 {

  class OrderedPoint extends java.awt.Point with scala.math.Ordered[java.awt.Point] {
    override def compare(that: Point): Int = {
      if (getX < that.getX) -1
      else if (getX == that.getX) {
        if (getY < that.getY) -1
        else if (getY == that.getY) 0
        else 1
      }
      else 1
    }
  }

}
