package fi.kapsi.kosmik.sfti.ch06

/**
  * Write an object Conversions with methods inchesToCentimeters, gallonsToLiters, and
  * milesToKilometers.
  */
//noinspection ScalaFileName
object Conversions {
  def inchesToCentimeters(inches: Double): Double = inches * 2.54

  def gallonsToLiters(gallons: Double): Double = gallons * 4.54609

  def milesToKilometers(miles: Double): Double = miles * 1609.344
}
