package fi.kapsi.kosmik.sfti.ch08

/**
  * Define an abstract class Shape with an abstract method centerPoint and subclasses
  * Rectangle and Circle . Provide appropriate constructors for the subclasses and
  * override the centerPoint method in each subclass.
  */
object Ex06 {

  type Coord = (Double, Double)

  abstract class Shape {
    def centerPoint: Coord
  }

  class Rectangle(val bottomLeft: Coord, val topRight: Coord) extends Shape {
    override def centerPoint: Coord = {
      (bottomLeft._1 + (topRight._1 - bottomLeft._1) / 2, bottomLeft._2 + (topRight._2 - bottomLeft._2) / 2)
    }
  }

  class Circle(val centerPoint: Coord, val radius: Double) extends Shape
}
