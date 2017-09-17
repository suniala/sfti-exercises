package fi.kapsi.kosmik.sfti.ch06

import org.scalatest.{FunSpec, Matchers}

class Chapter06Spec extends FunSpec with Matchers {
  describe("Exercise 06") {
    it("should produce suit icon") {
      Ex06Suit.Clubs.toString shouldEqual "♣"
    }
  }

  describe("Exercise 07") {
    import fi.kapsi.kosmik.sfti.ch06.Ex07Suit._
    import fi.kapsi.kosmik.sfti.ch06.Ex07._

    it("should give suit colour") {
      isRed(Diamonds) shouldEqual true
      isRed(Hearts) shouldEqual true
      isRed(Spades) shouldEqual false
      isRed(Clubs) shouldEqual false
    }
  }
}
