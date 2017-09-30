package fi.kapsi.kosmik.sfti.ch08

/**
  * Design a class Point whose x and y coordinate values can be provided in a
  * constructor. Provide a subclass LabeledPoint whose constructor takes a label
  * value and x and y coordinates, such as
  * new LabeledPoint("Black Thursday", 1929, 230.07)
  */
object Ex05 {

  class Point(val x: Double, val y: Double)

  class LabeledPoint(val label: String, override val x: Double, override val y: Double) extends Point(x, y)

}
