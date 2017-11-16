package fi.kapsi.kosmik.sfti

import org.scalatest.{FunSpec, Matchers}

class Chapter14Spec extends FunSpec with Matchers {
  describe("Exercise 02") {
    import fi.kapsi.kosmik.sfti.Chapter14.Ex02._

    it("should swap pair items") {
      swap((1, 2)) shouldEqual(2, 1)
    }
  }

  describe("Exercise 03") {
    import fi.kapsi.kosmik.sfti.Chapter14.Ex03._

    it("should swap first two elements") {
      swap(Array()) shouldEqual Array()
      swap(Array(2)) shouldEqual Array(2)
      swap(Array(4, 2)) shouldEqual Array(2, 4)
      swap(Array(4, 2, 5)) shouldEqual Array(2, 4, 5)
      swap(Array(4, 2, 5, 8)) shouldEqual Array(2, 4, 5, 8)
    }
  }

  describe("Exercise 04") {
    import fi.kapsi.kosmik.sfti.Chapter14.Ex04._

    val reikäleipä = Article("Reikäleipä", 3.20)
    val arinarievä = Article("Arinariävä", 2.59)
    val familyPack = Bundle("Family Pack", 0.5, reikäleipä, arinarievä)

    it("should calculate Article price") {
      price(reikäleipä) shouldEqual 3.20
    }

    it("should calculate Bundle price") {
      price(familyPack) shouldEqual 5.29
    }

    it("should calculate Multiple price") {
      price(Multiple(2, familyPack)) shouldEqual 10.58
    }
  }

  describe("Exercise 05") {
    import fi.kapsi.kosmik.sfti.Chapter14.Ex05._

    it("should calculate leaf sum") {
      val tree = (3 :: 8 :: 4 :: 2 :: Nil) :: 2 :: (5 :: Nil) :: Nil
      leafSum(tree) shouldEqual 24
    }
  }
}
