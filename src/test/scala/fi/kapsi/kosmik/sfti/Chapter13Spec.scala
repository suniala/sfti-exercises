package fi.kapsi.kosmik.sfti

import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ListBuffer

class Chapter13Spec extends FunSpec with Matchers with ExerciseSupport {

  private def charIndexesTestCase(indexes: String => collection.Map[Char, collection.SortedSet[Int]]): Unit = {
    // NOTE: Compare each set separately as we need to use toSeq for verifying correct order of elements.
    val actual = indexes("Mississippi")
    actual.size shouldEqual 4
    actual('M').toSeq shouldEqual Seq(0)
    actual('i').toSeq shouldEqual Seq(1, 4, 7, 10)
    actual('s').toSeq shouldEqual Seq(2, 3, 5, 6)
    actual('p').toSeq shouldEqual Seq(8, 9)
  }

  describe("Exercise 01") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex01._

    it("should produce indexes with mutable collections") {
      charIndexesTestCase(indexes)
    }
  }

  describe("Exercise 02") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex02._

    it("should produce indexes with immutable collections") {
      charIndexesTestCase(indexes)
    }
  }

  describe("Exercise 03") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex03._

    def input: ListBuffer[Int] = (1 to 10000).to[ListBuffer]

    val expected: ListBuffer[Int] = (1 to 10000 by 2).to[ListBuffer]

    it("should remove every second element in place") {
      val d = input
      val actual = evaluateAndTime("in place") {
        removeEverySecondInPlace(d)
      }
      actual shouldEqual expected
    }

    it("should remove every second element by copying") {
      val d = input
      val actual = evaluateAndTime("copying") {
        removeEverySecondCopy(d)
      }
      actual shouldEqual expected
    }
  }

  describe("Exercise 04") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex04._

    it("should lookup values using matching keys") {
      lookup(Array("Tom", "Fred", "Harry"), Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)) shouldEqual Array(3, 5)
    }
  }

  describe("Exercise 05") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex05._

    it("should make string") {
      mkString(List("yy", "kaa", "koo", "nee"), "...") shouldEqual "yy...kaa...koo...nee"
    }
  }

  describe("Exercise 06") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex06._

    it("should copy list by fold right") {
      copyByFoldRight(List(1, 2, 3, 4)) shouldEqual List(1, 2, 3, 4)
    }

    it("should copy list by fold left") {
      copyByFoldLeft(List(1, 2, 3, 4)) shouldEqual List(1, 2, 3, 4)
    }

    it("should reverse list by fold right") {
      reverseByFoldRight(List(1, 2, 3, 4)) shouldEqual List(4, 3, 2, 1)
    }

    it("should reverse list by fold left") {
      reverseByFoldLeft(List(1, 2, 3, 4)) shouldEqual List(4, 3, 2, 1)
    }
  }

  describe("Exercise 07") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex07._

    it("should produce same result with tupled") {
      val prices = List(5.0, 20.0, 9.95)
      val quantities = List(10, 2, 1)

      multiplyTuplesCase(prices, quantities) shouldEqual multiplyTuplesInelegant(prices, quantities)
      multiplyTuplesTupledWildcards(prices, quantities) shouldEqual multiplyTuplesInelegant(prices, quantities)
      multiplyTuplesTupledUtilityFunction(prices, quantities) shouldEqual multiplyTuplesInelegant(prices, quantities)
      multiplyTuplesZipped(prices, quantities) shouldEqual multiplyTuplesInelegant(prices, quantities)
    }
  }

  describe("Exercise 08") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex08._

    it("should wrap values") {
      wrap(Array(1, 2, 3, 4, 5, 6), 3) shouldEqual Array(Array(1, 2, 3), Array(4, 5, 6))
    }
  }

  describe("Exercise 09") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex09._

    it("should produce identical lists by mapping or for-looping over two iterators") {
      val mapResult = twoIterablesMap(10, 5)
      mapResult.length shouldEqual 10 * 5
      mapResult shouldEqual twoIterablesFor(10, 5)
    }

    it("should produce identical lists by mapping or for-looping over three iterators") {
      val mapResult = threeIterablesMap(3, 2, 5)
      mapResult.length shouldEqual 3 * 2 * 5
      mapResult shouldEqual threeIterablesFor(3, 2, 5)
    }
  }

  describe("Exercise 10") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex10._

    it("should find continent with most time zones") {
      continentWithMostTimeZones() shouldEqual "Arctic"
    }
  }

  describe("Exercise 11") {
    import fi.kapsi.kosmik.sfti.Chapter13.Ex11._

    val longString: String = (for (i <- 1 to 43; j <- 1 to 213) yield i + j).mkString("")
    lazy val expectedLongStringFrecs = evaluateAndTime("single thread") {
      frequenciesSingleThread(longString)
    }

    it("should calc frequencies using single thread reference implementation") {
      frequenciesSingleThread("diggiloo diggiley") shouldEqual Map(
        'd' -> 2,
        'i' -> 4,
        'g' -> 4,
        'l' -> 2,
        'o' -> 2,
        ' ' -> 1,
        'e' -> 1,
        'y' -> 1
      )
    }

    it("should not calc frequencies properly using faulty parallel implementation") {
      val frequencies = evaluateAndTime("faulty parallel") {
        frequenciesHackerParallel(longString)
      }
      // Works on my machine! Or more precisely, frequenciesHackerParallel does NOT work!
      frequencies shouldNot be(expectedLongStringFrecs)
    }

    it("should calc frequencies properly using fixed parallel implementation") {
      val frequencies = evaluateAndTime("fixed parallel") {
        frequenciesFixedParallel(longString)
      }
      frequencies should be(expectedLongStringFrecs)
    }
  }
}
