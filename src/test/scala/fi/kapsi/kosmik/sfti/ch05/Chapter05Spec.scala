package fi.kapsi.kosmik.sfti.ch05

import fi.kapsi.kosmik.sfti.ch05.Chapter05._
import org.scalatest.{FunSpec, Matchers}

class Chapter05Spec extends FunSpec with Matchers {
  describe("Exercise 03") {
    it("should evaluate \"before\"") {
      val first = new Ex03Time(1, 56)
      val second = new Ex03Time(15, 43)
      val third = new Ex03Time(15, 44)

      first.before(second) shouldEqual true
      second.before(third) shouldEqual true
      third.before(second) shouldEqual false
    }
  }

  describe("Exercise 04") {
    val first = new Ex04Time(1, 56)
    val second = new Ex04Time(15, 43)
    val third = new Ex04Time(15, 44)

    it("should map internal presentation to external") {
      first.toString shouldEqual "1:56"
      second.toString shouldEqual "15:43"
      third.toString shouldEqual "15:44"
    }

    it("should evaluate \"before\"") {
      first.before(second) shouldEqual true
      second.before(third) shouldEqual true
      third.before(second) shouldEqual false
    }
  }

  describe("Exercise 05") {
    it("should be able to call javabean methods in scala code") {
      val s = new Ex05Student(42, "Matti")
      s.id shouldEqual 42
      s.name shouldEqual "Matti"

      s.getId shouldEqual 42
      s.getName shouldEqual "Matti"

      s.id = 43
      s.name = "Teppo"

      s.setId(44)
      s.setName("Seppo")
    }
  }

  describe("Exercise 06") {
    it("should allow positive age") {
      val p = new Ex06Person(12)
      p.age shouldEqual 12
    }

    it("should default negative age to zero") {
      val p = new Ex06Person(-12)
      p.age shouldEqual 0
    }
  }

  describe("Exercise 07") {
    it("should parse formatted name") {
      val p = new Ex07Person("Minna Canth")
      p.firstName shouldEqual "Minna"
      p.lastName shouldEqual "Canth"
    }
  }

  describe("Exercise 08") {
    it("should construct with two parameters") {
      val car = new Ex08Car("Lada", "Niva")
      car.manufacturer shouldEqual "Lada"
      car.model shouldEqual "Niva"
      car.year shouldEqual -1
      car.license shouldEqual ""
    }

    it("should construct with three parameters one of which is year") {
      val car = new Ex08Car("Lada", "Niva", 1975)
      car.manufacturer shouldEqual "Lada"
      car.model shouldEqual "Niva"
      car.year shouldEqual 1975
      car.license shouldEqual ""
    }

    it("should construct with three parameters one of which is license") {
      val car = new Ex08Car("Lada", "Niva", "ABC-123")
      car.manufacturer shouldEqual "Lada"
      car.model shouldEqual "Niva"
      car.year shouldEqual -1
      car.license shouldEqual "ABC-123"
    }

    it("should construct with four parameters") {
      val car = new Ex08Car("Lada", "Niva", 1975, "ABC-123")
      car.manufacturer shouldEqual "Lada"
      car.model shouldEqual "Niva"
      car.year shouldEqual 1975
      car.license shouldEqual "ABC-123"
    }

    it("should be able to write license") {
      val car = new Ex08Car("Lada", "Niva")
      car.license shouldEqual ""
      car.license = "DEF-456"
      car.license shouldEqual "DEF-456"
    }
  }
}
