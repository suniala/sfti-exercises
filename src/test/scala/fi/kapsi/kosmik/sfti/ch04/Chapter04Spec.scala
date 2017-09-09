package fi.kapsi.kosmik.sfti.ch04

import java.io.InputStream

import fi.kapsi.kosmik.sfti.ch04.Chapter04._
import org.scalatest.{FunSpec, Matchers}

import scala.collection.SortedMap

class Chapter04Spec extends FunSpec with Matchers {
  describe("Exercise 01") {
    it("should apply discount") {
      val gizmos = Map("Ludwig Supraphonic" -> 200.0, "Bosphorus Special3 China" -> 180.0)
      val discounted = ex01(gizmos)
      discounted should have size gizmos.size
      discounted("Ludwig Supraphonic") shouldEqual 180.0
      discounted("Bosphorus Special3 China") shouldEqual 162.0
    }
  }

  describe("Exercise 02") {
    it("should produce a map with word counts") {
      ex02(getClass.getResourceAsStream("words.txt")) shouldEqual
        Map("hauki" -> 2, "lohi" -> 1, "nieriä" -> 1, "särki" -> 2, "ahven" -> 3)
    }
  }

  describe("Exercise 03") {
    it("should produce a map with word counts") {
      ex03(getClass.getResourceAsStream("words.txt")) shouldEqual
        Map("hauki" -> 2, "lohi" -> 1, "nieriä" -> 1, "särki" -> 2, "ahven" -> 3)
    }
  }

  describe("Exercise 04") {
    it("should produce a map with word counts in sorted order") {
      val counts = ex04(getClass.getResourceAsStream("words.txt"))
      counts shouldEqual Map("ahven" -> 3, "hauki" -> 2, "lohi" -> 1, "nieriä" -> 1, "särki" -> 2)
      counts.keys.toList should contain inOrderOnly("ahven", "hauki", "lohi", "nieriä", "särki")
    }
  }

  describe("Exercise 05") {
    it("should produce a map with word counts in sorted order") {
      val counts = ex05(getClass.getResourceAsStream("words.txt"))
      counts shouldEqual Map("ahven" -> 3, "hauki" -> 2, "lohi" -> 1, "nieriä" -> 1, "särki" -> 2)
      counts.keys.toList should contain inOrderOnly("ahven", "hauki", "lohi", "nieriä", "särki")
    }
  }

  describe("Exercise 06") {
    it("should produce a linked map in insertion order") {
      ex06(List(("Monday", 1), ("Tuesday", 2), ("Wednesday", 3), ("Thursday", 4))).values
        .toList should contain inOrderOnly(1, 2, 3, 4)
    }
  }

  describe("Exercise 07") {
    it("should print system props (check console)") {
      ex07()
    }
  }

  describe("Exercise 08") {
    it("should find smallest and largest number") {
      ex08_minmax(Array(5, -2, 9, 5, -7)) shouldEqual(-7, 9)
    }
  }

  describe("Exercise 09") {
    it("should produce a tuple of counts") {
      ex09_lteqgt(Array(1, 5, 4, 2, 12, 4, 2, 9, 7), 4) shouldEqual(3, 2, 4)
    }
  }

  describe("Exercise 10") {
    it("should zip strings") {
      ex10("Asdf", "Qwer") shouldEqual Vector(('A', 'Q'), ('s', 'w'), ('d', 'e'), ('f', 'r'))
    }
  }
}
