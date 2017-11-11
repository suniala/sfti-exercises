package fi.kapsi.kosmik.sfti

import org.scalatest.{FunSpec, Matchers}

class Chapter04Spec extends FunSpec with Matchers {
  describe("Exercise 01") {
    import fi.kapsi.kosmik.sfti.Chapter04.Ex01._

    it("should apply discount") {
      val gizmos = Map("Ludwig Supraphonic" -> 200.0, "Bosphorus Special3 China" -> 180.0)
      val discounted = discount(gizmos)
      discounted should have size gizmos.size
      discounted("Ludwig Supraphonic") shouldEqual 180.0
      discounted("Bosphorus Special3 China") shouldEqual 162.0
    }
  }

  describe("Exercise 02") {
    import fi.kapsi.kosmik.sfti.Chapter04.Ex02.countWords

    it("should produce a map with word counts") {
      countWords(getClass.getResourceAsStream("words.txt")) shouldEqual
        Map("hauki" -> 2, "lohi" -> 1, "nieriä" -> 1, "särki" -> 2, "ahven" -> 3)
    }
  }

  describe("Exercise 03") {
    import fi.kapsi.kosmik.sfti.Chapter04.Ex03.countWords

    it("should produce a map with word counts") {
      countWords(getClass.getResourceAsStream("words.txt")) shouldEqual
        Map("hauki" -> 2, "lohi" -> 1, "nieriä" -> 1, "särki" -> 2, "ahven" -> 3)
    }
  }

  describe("Exercise 04") {
    import fi.kapsi.kosmik.sfti.Chapter04.Ex04.countWords

    it("should produce a map with word counts in sorted order") {
      val counts = countWords(getClass.getResourceAsStream("words.txt"))
      counts shouldEqual Map("ahven" -> 3, "hauki" -> 2, "lohi" -> 1, "nieriä" -> 1, "särki" -> 2)
      counts.keys.toList should contain inOrderOnly("ahven", "hauki", "lohi", "nieriä", "särki")
    }
  }

  describe("Exercise 05") {
    import fi.kapsi.kosmik.sfti.Chapter04.Ex05.countWords

    it("should produce a map with word counts in sorted order") {
      val counts = countWords(getClass.getResourceAsStream("words.txt"))
      counts shouldEqual Map("ahven" -> 3, "hauki" -> 2, "lohi" -> 1, "nieriä" -> 1, "särki" -> 2)
      counts.keys.toList should contain inOrderOnly("ahven", "hauki", "lohi", "nieriä", "särki")
    }
  }

  describe("Exercise 06") {
    import fi.kapsi.kosmik.sfti.Chapter04.Ex06.toLinkedMap

    it("should produce a linked map in insertion order") {
      toLinkedMap(List(("Monday", 1), ("Tuesday", 2), ("Wednesday", 3), ("Thursday", 4))).values
        .toList should contain inOrderOnly(1, 2, 3, 4)
    }
  }

  describe("Exercise 07") {
    import fi.kapsi.kosmik.sfti.Chapter04.Ex07._

    it("should print system props (check console)") {
      val formattedProps = systemProps(printAlso = false)
      // Only do simple assertions. Formatting should be checked manually.
      formattedProps.contains("java.version") shouldBe true
      formattedProps.contains("java.vm.specification.version") shouldBe true
    }
  }

  describe("Exercise 08") {
    import fi.kapsi.kosmik.sfti.Chapter04.Ex08._

    it("should find smallest and largest number") {
      minmax(Array(5, -2, 9, 5, -7)) shouldEqual(-7, 9)
    }
  }

  describe("Exercise 09") {
    import fi.kapsi.kosmik.sfti.Chapter04.Ex09._

    it("should produce a tuple of counts") {
      lteqgt(Array(1, 5, 4, 2, 12, 4, 2, 9, 7), 4) shouldEqual(3, 2, 4)
    }
  }

  describe("Exercise 10") {
    import fi.kapsi.kosmik.sfti.Chapter04.Ex10._

    it("should zip strings") {
      zip("Asdf", "Qwer") shouldEqual Vector(('A', 'Q'), ('s', 'w'), ('d', 'e'), ('f', 'r'))
    }
  }
}
