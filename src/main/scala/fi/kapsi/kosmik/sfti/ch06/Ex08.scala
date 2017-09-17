package fi.kapsi.kosmik.sfti.ch06

/**
  * Write an enumeration describing the eight corners of the RGB color cube. As
  * IDs, use the color values (for example, 0xff0000 for Red ).
  */
//noinspection ScalaFileName
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
