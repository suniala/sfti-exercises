package fi.kapsi.kosmik.sfti.ch03

import fi.kapsi.kosmik.sfti.ch03.Chapter03._
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class Chapter03Spec extends FunSpec with Matchers {
  describe("Exercise 02") {
    it("should swap items") {
      ex02(Array(1, 2)) shouldEqual Array(2, 1)
      ex02(Array(1, 2, 3)) shouldEqual Array(2, 1, 3)
      ex02(Array(1, 2, 3, 4, 5)) shouldEqual Array(2, 1, 4, 3, 5)
    }

    it("should do swapping in-place") {
      val array = Array(1, 2, 3)
      ex02(array) should be theSameInstanceAs array
    }
  }

  describe("Exercise 03") {
    it("should swap items") {
      ex03(Array(1, 2)) shouldEqual Array(2, 1)
      ex03(Array(1, 2, 3)) shouldEqual Array(2, 1, 3)
      ex03(Array(1, 2, 3, 4, 5)) shouldEqual Array(2, 1, 4, 3, 5)
    }

    it("should create a new instance") {
      val array = Array(1, 2, 3)
      ex03(array) shouldNot be theSameInstanceAs array
    }
  }

  describe("Exercise 04") {
    it("should produce a new array with specific ordering") {
      ex04(Array(4, -4, 2, -1, 3)) shouldEqual Array(4, 2, 3, -4, -1)
      ex04(Array(1, 2, 3)) shouldEqual Array(1, 2, 3)
    }
  }

  describe("Exercise 05") {
    it("should produce average") {
      ex05(Array(1.0, 3.0)) shouldEqual 2.0
      ex05(Array(1.0, 3.0, 1.5, 9.4)) shouldEqual 3.725
    }
  }

  describe("Exercise 06 a") {
    it("should sort in reverse order") {
      ex06a(Array(4, -4, 2, -1, 3)) shouldEqual Array(4, 3, 2, -1, -4)
    }
  }

  describe("Exercise 06 b") {
    it("should sort in reverse order") {
      ex06b(ArrayBuffer(4, -4, 2, -1, 3)) shouldEqual ArrayBuffer(4, 3, 2, -1, -4)
    }
  }

  describe("Exercise 07") {
    it("should produce an array with unique values only") {
      ex07(Array(1)) shouldEqual Array(1)
      ex07(Array(1, 6, 3, 5, 1, 5)) shouldEqual Array(1, 6, 3, 5)
    }
  }

  describe("Exercise 08") {
    it("should remove (in-place) all but first negative number from array buffer") {
      ex08(ArrayBuffer(1, 6, -3, -2, 5, 1, -6, 5, -1)) shouldEqual ArrayBuffer(1, 6, -3, 5, 1, 5)
    }
  }

  describe("Exercise 09") {
    it("should remove (in-place) all but first negative number from array buffer") {
      ex08(ArrayBuffer(1, 6, -3, -2, 5, 1, -6, 5, -1, 1)) shouldEqual ArrayBuffer(1, 6, -3, 5, 1, 5, 1)
      ex08(ArrayBuffer(1, 6, -3, -2, 5, 1, -6, 5, -1)) shouldEqual ArrayBuffer(1, 6, -3, 5, 1, 5)
      ex08(ArrayBuffer(-3, -2, 5, 1, -6, 5, -1)) shouldEqual ArrayBuffer(-3, 5, 1, 5)
    }
  }
}
