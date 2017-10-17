package fi.kapsi.kosmik.sfti.ch11

/**
  * Implement a class Money with fields for dollars and cents. Supply + , - operators
  * as well as comparison operators == and < . For example, Money(1, 75) + Money(0,
  * 50) == Money(2, 25) should be true . Should you also supply * and / operators?
  * Why or why not?
  */
object Ex04 {

  class Money(val dollarSum: Int, val centSum: Int) {
    val dollars: Int = dollarSum + centSum / 100
    val cents: Int = centSum - centSum / 100 * 100

    def +(other: Money): Money = Money(0, 100 * (dollars + other.dollars) + cents + other.cents)

    def -(other: Money): Money = Money(0, 100 * (dollars - other.dollars) + cents - other.cents)

    final override def equals(other: Any): Boolean = {
      other match {
        case that: Money => dollars == that.dollars && cents == that.cents
        case _ => false
      }
    }

    def <(other: Money): Boolean =
      if (dollars < other.dollars) true
      else if (dollars == other.dollars) cents < other.cents
      else false

    override def toString: String = s"$$$dollars.$cents"

    /**
      * Provide an explicit multiplication method. A multiplier operator can not be used here as the other operand
      * is of another type (in this case Double) and operations like "2.0 * Money(2, 43)" would not work as this
      * operator associates to the left.
      *
      * @param multiplier multiply with this
      * @return multiplied money
      */
    def mul(multiplier: Double): Money =
      Money(0, math.round(multiplier * (100 * dollars + cents)).toInt)
  }

  object Money {
    def apply(dollarSum: Int, centSum: Int) = new Money(dollarSum, centSum)
  }

}
