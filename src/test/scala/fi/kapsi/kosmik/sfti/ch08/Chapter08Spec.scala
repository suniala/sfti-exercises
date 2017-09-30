package fi.kapsi.kosmik.sfti.ch08

import fi.kapsi.kosmik.sfti.ch08.Ex01.CheckingAccount
import fi.kapsi.kosmik.sfti.ch08.Ex02.SavingsAccount
import fi.kapsi.kosmik.sfti.ch08.Ex04.{Bundle, SimpleItem}
import fi.kapsi.kosmik.sfti.ch08.Ex05.LabeledPoint
import fi.kapsi.kosmik.sfti.ch08.Ex06.{Circle, Rectangle}
import fi.kapsi.kosmik.sfti.ch08.Ex07.Square
import fi.kapsi.kosmik.sfti.ch08.Ex09.{AntA, AntB, AntC}
import org.scalatest.{FunSpec, Matchers}

class Chapter08Spec extends FunSpec with Matchers {
  describe("Exercise 01") {
    it("should add transaction fee") {
      val account = new CheckingAccount(100.0)
      account.currentBalance shouldEqual 100.0

      account.deposit(55.34) shouldEqual 154.34
      account.currentBalance

      account.withdraw(25.12) shouldEqual 128.22
    }
  }

  describe("Exercise 02") {
    it("should add transaction fee after third transaction in month") {
      val account = new SavingsAccount(100.0)
      account.currentBalance shouldEqual 100.0

      account.deposit(55.34) shouldEqual 155.34

      account.withdraw(25.12) shouldEqual 130.22

      account.withdraw(1.0) shouldEqual 129.22

      account.withdraw(1.22) shouldEqual 127.00

      account.earnMonthlyInterest(0.01) shouldEqual 128.27 +- 0.01

      account.withdraw(1.0) shouldEqual 127.27 +- 0.01
    }
  }

  describe("Exercise 04") {
    it("should compute prices and descriptions of bundles") {
      val hammer = new SimpleItem(12.34, "Hammer")
      val nail1 = new SimpleItem(1.99, "Indestructible nail")
      val nail2 = new SimpleItem(1.99, "Indestructible nail")

      hammer.price shouldEqual 12.34

      val nails = new Bundle()
      nails
        .add(nail1)
        .add(nail2)

      val bundle = new Bundle()
      bundle
        .add(hammer)
        .add(nails)

      bundle.price shouldEqual 16.32
      bundle.description shouldEqual "Hammer, Indestructible nail, Indestructible nail"
    }
  }

  describe("Exercise 05") {
    it("should implement constructor") {
      val point = new LabeledPoint("Black Thursday", 1929, 230.07)
      point.x shouldEqual 1929
    }
  }

  describe("Exercise 06") {
    it("should calculate rectangle center point") {
      val rect = new Rectangle((-1, -1), (2, 2))
      rect.centerPoint shouldEqual (0.5, 0.5)
    }

    it("should give circle center point") {
      val circle = new Circle((-1, 2), 3.14)
      circle.centerPoint shouldEqual (-1, 2)
    }
  }

  describe("Exercise 07") {
    it("should construct with coord and width") {
      val sq = new Square(1, 2, 4)
      sq.height shouldEqual sq.width
      sq.height shouldEqual 4
    }

    it("should construct with width") {
      val sq = new Square(4)
      sq.height shouldEqual sq.width
      sq.height shouldEqual 4
      sq.x shouldEqual 0
      sq.y shouldEqual 0
    }

    it("should construct a point square") {
      val sq = new Square()
      sq.height shouldEqual sq.width
      sq.height shouldEqual 0
      sq.x shouldEqual 0
      sq.y shouldEqual 0
    }
  }

  describe("Exercise 09") {
    it("should show fault in AntA") {
      val ant = new AntA()
      ant.range shouldEqual 2
      // AntA.env is an empty array due to a fault in construction sequence
      ant.env shouldEqual Array()
    }

    it("should show fault in AntB") {
      val ant = new AntB()
      ant.range shouldEqual 2
      // env is still initialized as an empty array since the getter method for field AntB.range
      // overrides CreatureB.range method and thus returns the uninitialized (default) value range.
      ant.env shouldEqual Array()
    }

    it("should show AntC is constructed properly") {
      val ant = new AntC()
      ant.range shouldEqual 2
      // env is initialized correctly as AntC.range is now a method which does not need construction
      // time initialization so as to return the proper value.
      ant.env shouldEqual Array(0, 0)
    }
  }
}
