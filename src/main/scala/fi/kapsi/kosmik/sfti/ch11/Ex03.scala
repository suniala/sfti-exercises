package fi.kapsi.kosmik.sfti.ch11

/**
  * Implement the Fraction class with operations + - * / . Normalize fractions, for
  * example, turning 15/–6 into –5/2. Divide by the greatest common divisor
  */
object Ex03 {

  class Fraction(n: Int, d: Int) {
    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)

    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)

    override def toString = s"$num/$den"

    def sign(a: Int): Int = if (a > 0) 1 else if (a < 0) -1 else 0

    def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)

    final override def equals(obj: scala.Any): Boolean = {
      obj match {
        case other: Fraction => num == other.num && den == other.den
        case _ => false
      }
    }

    def +(other: Fraction): Fraction = Fraction(num * other.den + other.num * den, den * other.den)

    def -(other: Fraction): Fraction = Fraction(num * other.den - other.num * den, den * other.den)

    def unary_-(): Fraction = Fraction(-num, den)

    def *(other: Fraction): Fraction = Fraction(num * other.num, den * other.den)

    def /(other: Fraction): Fraction = {
      val otherCommonDen = other.num * other.den
      Fraction(num * otherCommonDen / other.num, den * otherCommonDen / other.den)
    }
  }

  object Fraction {
    def apply(n: Int, d: Int) = new Fraction(n, d)
  }

}
