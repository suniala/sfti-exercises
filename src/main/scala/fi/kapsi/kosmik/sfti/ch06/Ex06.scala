package fi.kapsi.kosmik.sfti.ch06

/**
  * Write an enumeration describing the four playing card suits so that the toString
  * method returns ♣, ♦, ♥, or ♠.
  */
//noinspection ScalaFileName
object Ex06Suit extends Enumeration {
  val Clubs = Value("♣")
  val Diamonds = Value("♦")
  val Hearts = Value("♥")
  val Spades = Value("♠")
}
