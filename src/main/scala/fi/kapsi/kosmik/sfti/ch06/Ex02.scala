package fi.kapsi.kosmik.sfti.ch06

/**
  * The preceding problem wasn’t very object-oriented. Provide a general super-
  * class UnitConversion and define objects InchesToCentimeters, GallonsToLiters, and
  * MilesToKilometers that extend it.
  */
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
