package fi.kapsi.kosmik.sfti

object Chapter06 {

  /**
    * Write an object Conversions with methods inchesToCentimeters, gallonsToLiters, and
    * milesToKilometers.
    */
  object Ex01 {

    object Conversions {
      def inchesToCentimeters(inches: Double): Double = inches * 2.54

      def gallonsToLiters(gallons: Double): Double = gallons * 4.54609

      def milesToKilometers(miles: Double): Double = miles * 1609.344
    }

  }

  /**
    * The preceding problem wasn’t very object-oriented. Provide a general super-
    * class UnitConversion and define objects InchesToCentimeters, GallonsToLiters, and
    * MilesToKilometers that extend it.
    */
  object Ex02 {

    abstract class UnitConversion {
      def convert(value: Double): Double
    }

    object InchesToCentimeters extends UnitConversion {
      override def convert(value: Double): Double = value * 2.54
    }

    object GallonsToLiters extends UnitConversion {
      override def convert(value: Double): Double = value * 4.54609
    }

    object MilesToKilometers extends UnitConversion {
      override def convert(value: Double): Double = value * 1609.344
    }

  }

  /**
    * Define an Origin object that extends java.awt.Point. Why is this not actually a
    * good idea? (Have a close look at the methods of the Point class.)
    *
    * Answer: Point, and thus Ex03Origin, is mutable.
    */
  object Ex03 {

    object Origin extends java.awt.Point(0, 0)

  }

  /**
    * Define a Point class with a companion object so that you can construct Point
    * instances as Point(3, 4), without using new.
    */
  object Ex04 {

    class Point(val x: Int, val y: Int)

    object Point {
      def apply(x: Int, y: Int): Point = new Point(x, y)
    }

    object Example {
      val point: Point = Point(1, 2)
    }

  }

  /**
    * Write a Scala application, using the App trait, that prints its command-line
    * arguments in reverse order, separated by spaces. For example, scala Reverse
    * Hello World should print World Hello .
    */
  object Ex05 {

    object Reverse extends App {
      println(args.reverse.mkString(" "))
    }

  }

  /**
    * Write an enumeration describing the four playing card suits so that the toString
    * method returns ♣, ♦, ♥, or ♠.
    */
  object Ex06 {

    object Suit extends Enumeration {
      val Clubs = Value("♣")
      val Diamonds = Value("♦")
      val Hearts = Value("♥")
      val Spades = Value("♠")
    }

  }

  /**
    * Implement a function that checks whether a card suit value from the preceding
    * exercise is red.
    */
  object Ex07 {

    object Suit extends Enumeration {
      // NOTE: Introduce type alias for Value so as to avoid having to write Ex07Suit.Value.
      type Suit = Value

      val Clubs = Value("♣")
      val Diamonds = Value("♦")
      val Hearts = Value("♥")
      val Spades = Value("♠")
    }

    import fi.kapsi.kosmik.sfti.Chapter06.Ex07.Suit._

    def isRed(suit: Suit): Boolean = {
      if (suit == Diamonds || suit == Hearts) true
      else false
    }
  }

  /**
    * Write an enumeration describing the eight corners of the RGB color cube. As
    * IDs, use the color values (for example, 0xff0000 for Red ).
    */
  object Ex08 {

    object RGBCorner extends Enumeration {
      val Red = Value(0xff0000, "Red")
      val Green = Value(0x00ff00, "Green")
      val Blue = Value(0x0000ff, "Blue")
      val Yellow = Value(0xffff00, "Yellow")
      val Violet = Value(0xff00ff, "Violet")
      val Black = Value(0x000000, "Black")
      val Turquoise = Value(0x00ffff, "Turquoise")
      val White = Value(0xffffff, "White")
    }

  }

}