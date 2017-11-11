package fi.kapsi.kosmik.sfti

import org.scalatest.{FunSpec, Matchers}

class Chapter05Spec extends FunSpec with Matchers {
  describe("Exercise 03") {
    import fi.kapsi.kosmik.sfti.Chapter05.Ex03._

    it("should evaluate \"before\"") {
      val first = new Time(1, 56)
      val second = new Time(15, 43)
      val third = new Time(15, 44)

      first.before(second) shouldEqual true
      second.before(third) shouldEqual true
      third.before(second) shouldEqual false
    }
  }

  describe("Exercise 04") {
    import fi.kapsi.kosmik.sfti.Chapter05.Ex04._

    val first = new Time(1, 56)
    val second = new Time(15, 43)
    val third = new Time(15, 44)

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
    import fi.kapsi.kosmik.sfti.Chapter05.Ex05._

    it("should be able to call javabean methods in scala code") {
      val s = new Student(42, "Matti")
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
    import fi.kapsi.kosmik.sfti.Chapter05.Ex06._

    it("should allow positive age") {
      val p = new Person(12)
      p.age shouldEqual 12
    }

    it("should default negative age to zero") {
      val p = new Person(-12)
      p.age shouldEqual 0
    }
  }

  describe("Exercise 07") {
    import fi.kapsi.kosmik.sfti.Chapter05.Ex07._

    it("should parse formatted name") {
      val p = new Person("Minna Canth")
      p.firstName shouldEqual "Minna"
      p.lastName shouldEqual "Canth"
    }
  }

  describe("Exercise 08") {
    import fi.kapsi.kosmik.sfti.Chapter05.Ex08._

    it("should construct with two parameters") {
      val car = new Car("Lada", "Niva")
      car.manufacturer shouldEqual "Lada"
      car.model shouldEqual "Niva"
      car.year shouldEqual -1
      car.license shouldEqual ""
    }

    it("should construct with three parameters one of which is year") {
      val car = new Car("Lada", "Niva", 1975)
      car.manufacturer shouldEqual "Lada"
      car.model shouldEqual "Niva"
      car.year shouldEqual 1975
      car.license shouldEqual ""
    }

    it("should construct with three parameters one of which is license") {
      val car = new Car("Lada", "Niva", "ABC-123")
      car.manufacturer shouldEqual "Lada"
      car.model shouldEqual "Niva"
      car.year shouldEqual -1
      car.license shouldEqual "ABC-123"
    }

    it("should construct with four parameters") {
      val car = new Car("Lada", "Niva", 1975, "ABC-123")
      car.manufacturer shouldEqual "Lada"
      car.model shouldEqual "Niva"
      car.year shouldEqual 1975
      car.license shouldEqual "ABC-123"
    }

    it("should be able to write license") {
      val car = new Car("Lada", "Niva")
      car.license shouldEqual ""
      car.license = "DEF-456"
      car.license shouldEqual "DEF-456"
    }
  }
}
