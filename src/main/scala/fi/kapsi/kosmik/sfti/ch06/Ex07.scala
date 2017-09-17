package fi.kapsi.kosmik.sfti.ch06

/**
  * Implement a function that checks whether a card suit value from the preceding
  * exercise is red.
  */
object Ex07Suit extends Enumeration {
  // NOTE: Introduce type alias for Value so as to avoid having to write Ex07Suit.Value.
  type Ex07Suit = Value

  val Clubs = Value("♣")
  val Diamonds = Value("♦")
  val Hearts = Value("♥")
  val Spades = Value("♠")
}

object Ex07 {

  import fi.kapsi.kosmik.sfti.ch06.Ex07Suit._

  def isRed(suit: Ex07Suit): Boolean = {
    if (suit == Diamonds || suit == Hearts) true
    else false
  }
}
