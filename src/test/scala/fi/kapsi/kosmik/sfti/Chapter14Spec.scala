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

  describe("Exercise 06") {
    import fi.kapsi.kosmik.sfti.Chapter14.Ex06._

    it("should calculate leaf sum") {
      val tree = Node(
        Node(
          Leaf(3),
          Leaf(8)
        ),
        Node(
          Leaf(2),
          Node(
            Leaf(5),
            Leaf(3)
          )
        )
      )

      leafSum(tree) shouldEqual 21
    }
  }

  describe("Exercise 07") {
    import fi.kapsi.kosmik.sfti.Chapter14.Ex07._

    it("should calculate leaf sum") {
      val tree =
        Node(
          Node(
            Leaf(3),
            Leaf(8)
          ),
          Leaf(2),
          Node(
            Leaf(5)
          )
        )

      leafSum(tree) shouldEqual 18
    }
  }

  describe("Exercise 08") {
    import fi.kapsi.kosmik.sfti.Chapter14.Ex08._

    it("should evaluate tree that includes unary minus") {
      val tree =
        Node(
          Sum(),
          Node(
            Mul(),
            Leaf(3),
            Leaf(8)
          ),
          Leaf(2),
          Node(
            Minus(),
            Leaf(5)
          )
        )

      eval(tree) shouldEqual ((3 * 8) + 2 + (-5))
    }

    it("should evaluate tree that includes binary minus") {
      val tree =
        Node(
          Minus(),
          Node(
            Mul(),
            Leaf(3),
            Leaf(8)
          ),
          Node(
            Minus(),
            Leaf(2),
            Leaf(7)
          )
        )

      eval(tree) shouldEqual (3 * 8 - (2 - 7))
    }
  }

  describe("Exercise 09") {
    import Chapter14.Ex09._

    it("should sum Some values") {
      sum(List(Some(3), Some(2))) shouldEqual 5
    }

    it("should ignore None values") {
      sum(List(Some(3), None, Some(2), Some(4), None)) shouldEqual 9
    }
  }

  describe("Exercise 10") {
    import Chapter14.Ex10._

    it("should compose two functions that may yield None") {
      def f(x: Double) = if (x != 1) Some(1 / (x - 1)) else None

      def g(x: Double) = if (x >= 0) Some(math.sqrt(x)) else None

      val h = compose(g, f)

      h(2) shouldEqual Some(1)
      h(1) shouldEqual None
      h(0) shouldEqual None
    }
  }
}
