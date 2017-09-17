package fi.kapsi.kosmik.sfti.ch06

/**
  * Define an Origin object that extends java.awt.Point. Why is this not actually a
  * good idea? (Have a close look at the methods of the Point class.)
  *
  * Answer: Point, and thus Ex03Origin, is mutable.
  */
//noinspection ScalaFileName
object Origin extends java.awt.Point(0, 0)
