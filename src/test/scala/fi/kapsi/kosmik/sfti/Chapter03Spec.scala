package fi.kapsi.kosmik.sfti

import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class Chapter03Spec extends FunSpec with Matchers {
  describe("Exercise 02") {
    import Chapter03.Ex02._

    it("should swap items") {
      swap(Array(1, 2)) shouldEqual Array(2, 1)
      swap(Array(1, 2, 3)) shouldEqual Array(2, 1, 3)
      swap(Array(1, 2, 3, 4, 5)) shouldEqual Array(2, 1, 4, 3, 5)
    }

    it("should do swapping in-place") {
      val array = Array(1, 2, 3)
      swap(array) should be theSameInstanceAs array
    }
  }

  describe("Exercise 03") {
    import Chapter03.Ex03._

    it("should swap items") {
      swap(Array(1, 2)) shouldEqual Array(2, 1)
      swap(Array(1, 2, 3)) shouldEqual Array(2, 1, 3)
      swap(Array(1, 2, 3, 4, 5)) shouldEqual Array(2, 1, 4, 3, 5)
    }

    it("should create a new instance") {
      val array = Array(1, 2, 3)
      swap(array) shouldNot be theSameInstanceAs array
    }
  }

  describe("Exercise 04") {
    import Chapter03.Ex04._

    it("should produce a new array with specific ordering") {
      partition(Array(4, -4, 2, -1, 3)) shouldEqual Array(4, 2, 3, -4, -1)
      partition(Array(1, 2, 3)) shouldEqual Array(1, 2, 3)
    }
  }

  describe("Exercise 05") {
    import Chapter03.Ex05._

    it("should produce average") {
      avg(Array(1.0, 3.0)) shouldEqual 2.0
      avg(Array(1.0, 3.0, 1.5, 9.4)) shouldEqual 3.725
    }
  }

  describe("Exercise 06") {
    import Chapter03.Ex06._

    it("should sort Array in reverse order") {
      reverseSort(Array(4, -4, 2, -1, 3)) shouldEqual Array(4, 3, 2, -1, -4)
    }

    it("should sort ArrayBuffer in reverse order") {
      reverseSort(ArrayBuffer(4, -4, 2, -1, 3)) shouldEqual ArrayBuffer(4, 3, 2, -1, -4)
    }
  }

  describe("Exercise 07") {
    import Chapter03.Ex07._

    it("should produce an array with unique values only") {
      uniq(Array(1)) shouldEqual Array(1)
      uniq(Array(1, 6, 3, 5, 1, 5)) shouldEqual Array(1, 6, 3, 5)
    }
  }

  describe("Exercise 08") {
    import Chapter03.Ex08._

    it("should remove (in-place) all but first negative number from array buffer") {
      removeAllButFirstNegative(ArrayBuffer(1, 6, -3, -2, 5, 1, -6, 5, -1)) shouldEqual ArrayBuffer(1, 6, -3, 5, 1, 5)
    }
  }

  describe("Exercise 09") {
    import Chapter03.Ex09._

    it("should remove (in-place) all but first negative number from array buffer") {
      removeAllButFirstNegative(ArrayBuffer(1, 6, -3, -2, 5, 1, -6, 5, -1, 1)) shouldEqual ArrayBuffer(1, 6, -3, 5, 1, 5, 1)
      removeAllButFirstNegative(ArrayBuffer(1, 6, -3, -2, 5, 1, -6, 5, -1)) shouldEqual ArrayBuffer(1, 6, -3, 5, 1, 5)
      removeAllButFirstNegative(ArrayBuffer(-3, -2, 5, 1, -6, 5, -1)) shouldEqual ArrayBuffer(-3, 5, 1, 5)
    }
  }

  describe("Exercise 10") {
    import Chapter03.Ex10._

    it("should return first 6 American time zones") {
      americanZones() shouldEqual Array("Adak", "Anchorage", "Anguilla", "Antigua", "Araguaina", "Argentina/Buenos_Aires")
    }
  }

  describe("Exercise 11") {
    import Chapter03.Ex11._

    it("should return a buffer with strings in it") {
      imageFlavors() should have length 6
    }
  }
}
