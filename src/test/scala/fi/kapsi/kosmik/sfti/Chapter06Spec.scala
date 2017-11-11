package fi.kapsi.kosmik.sfti

import org.scalatest.{FunSpec, Matchers}

class Chapter06Spec extends FunSpec with Matchers {
  describe("Exercise 06") {
    import fi.kapsi.kosmik.sfti.Chapter06.Ex06._

    it("should produce suit icon") {
      Suit.Clubs.toString shouldEqual "♣"
    }
  }

  describe("Exercise 07") {
    import fi.kapsi.kosmik.sfti.Chapter06.Ex07.Suit._
    import fi.kapsi.kosmik.sfti.Chapter06.Ex07._

    it("should give suit colour") {
      isRed(Diamonds) shouldEqual true
      isRed(Hearts) shouldEqual true
      isRed(Spades) shouldEqual false
      isRed(Clubs) shouldEqual false
    }
  }
}
