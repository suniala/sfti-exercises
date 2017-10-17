package fi.kapsi.kosmik.sfti.ch11

import fi.kapsi.kosmik.sfti.ch11.Ex03.Fraction
import fi.kapsi.kosmik.sfti.ch11.Ex04.Money
import fi.kapsi.kosmik.sfti.ch11.Ex05.Table
import fi.kapsi.kosmik.sfti.ch11.Ex06.{Cat, Hello}
import org.scalatest.{FunSpec, Matchers}

class Chapter11Spec extends FunSpec with Matchers {
  describe("Exercise 03") {
    it("should calculate sum") {
      Fraction(1, 4) + Fraction(1, 2) shouldEqual Fraction(3, 4)
      Fraction(5, 3) + Fraction(4, 7) shouldEqual Fraction(47, 21)
    }

    it("should calculate difference") {
      Fraction(5, 7) - Fraction(2, 7) shouldEqual Fraction(3, 7)
      Fraction(1, 4) - Fraction(1, 2) shouldEqual -Fraction(1, 4)
    }

    it("should calculate product") {
      Fraction(5, 2) * Fraction(2, 3) shouldEqual Fraction(5, 3)
      -Fraction(2, 4) * Fraction(1, 2) shouldEqual -Fraction(1, 4)
    }

    it("should calculate division") {
      Fraction(5, 2) / Fraction(2, 3) shouldEqual Fraction(15, 4)
    }
  }

  describe("Exercise 04") {
    it("should transfer excess cents to dollars") {
      Money(1, 0).dollars shouldEqual 1
      Money(1, 0).cents shouldEqual 0

      Money(1, 12).dollars shouldEqual 1
      Money(1, 12).cents shouldEqual 12

      Money(3, 152).dollars shouldEqual 4
      Money(3, 152).cents shouldEqual 52
    }

    it("should determine equality") {
      Money(1, 0) == Money(1, 0) shouldBe true
      Money(10, 54) == Money(9, 154) shouldBe true
      Money(1, 0) == Money(1, 1) shouldBe false
      Money(2, 1) == Money(1, 1) shouldBe false
    }

    it("should calculate sum") {
      Money(1, 0) + Money(2, 0) shouldEqual Money(3, 0)
      Money(0, 16) + Money(2, 89) shouldEqual Money(3, 5)
    }

    it("should calculate difference") {
      Money(1, 0) - Money(2, 0) shouldEqual Money(-1, 0)
      Money(3, 16) - Money(1, 89) shouldEqual Money(1, 27)
    }

    it("should determine which is less than the other") {
      Money(1, 0) < Money(2, 0) shouldBe true
      Money(1, 43) < Money(1, 44) shouldBe true
      Money(1, 44) < Money(1, 44) shouldBe false
      Money(2, 23) < Money(1, 44) shouldBe false
    }

    it("should multiply") {
      Money(1, 0).mul(2) shouldEqual Money(2, 0)
      Money(1, 32).mul(2.3) shouldEqual Money(3, 4)
      Money(5, 43).mul(0.6) shouldEqual Money(3, 26)
    }
  }

  describe("Exercise 05") {
    it("should build table markup with operators") {
      val table = Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"
      val markup = table.toString
      markup shouldEqual
        "<table><tr><td>Java</td><td>Scala</td></tr><tr><td>Gosling</td><td>Odersky</td></tr><tr><td>JVM</td><td>JVM, .NET</td></tr></table>"
    }
  }

  describe("Exercise 06") {
    def expected(name: String): String =
      scala.io.Source.fromInputStream(getClass.getResourceAsStream(name + ".txt")).getLines().mkString("\n")

    it("should add right") {
      (Cat() :- Hello()).toString shouldEqual expected("cat-with-hello-right")
    }

    it("should add right and below") {
      (Cat() :- Hello() :\ Hello()).toString shouldEqual expected("cat-with-hello-right-and-below")
    }

    it("should add below") {
      (Hello() :\ Cat()).toString shouldEqual expected("hello-with-cat-below")
    }
  }
}
