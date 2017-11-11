package fi.kapsi.kosmik.sfti

import fi.kapsi.kosmik.sfti.TestUtil.evaluateAndTime
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ListBuffer

class Chapter13Spec extends FunSpec with Matchers {

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
      val actual = evaluateAndTime(s => info(s), "in place") {
        removeEverySecondInPlace(d)
      }
      actual shouldEqual expected
    }

    it("should remove every second element by copying") {
      val d = input
      val actual = evaluateAndTime(s => info(s), "copying") {
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
}
