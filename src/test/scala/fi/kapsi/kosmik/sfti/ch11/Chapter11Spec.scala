package fi.kapsi.kosmik.sfti.ch11

import java.io.File

import fi.kapsi.kosmik.sfti.ch11.Ex03.Fraction
import fi.kapsi.kosmik.sfti.ch11.Ex04.Money
import fi.kapsi.kosmik.sfti.ch11.Ex05.Table
import fi.kapsi.kosmik.sfti.ch11.Ex06.{Cat, Hello}
import fi.kapsi.kosmik.sfti.ch11.Ex07.BitSequence
import fi.kapsi.kosmik.sfti.ch11.Ex08.Matrix
import fi.kapsi.kosmik.sfti.ch11.Ex09.PathComponents
import fi.kapsi.kosmik.sfti.ch11.Ex10.SeqPathComponents
import fi.kapsi.kosmik.sfti.ch11.Ex11.DynamicProps
import fi.kapsi.kosmik.sfti.ch11.Ex12.XMLElement
import fi.kapsi.kosmik.sfti.ch11.Ex13.XMLBuilder
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

  describe("Exercise 07") {
    it("should initialize a nil sequence") {
      val nils = BitSequence()

      nils(0) shouldEqual false
      nils(32) shouldEqual false
      nils(63) shouldEqual false
    }

    it("should update bits") {
      val bits = BitSequence()

      bits(0) = true
      bits(0) shouldBe true
      bits(1) shouldBe false

      bits(0) = false
      bits(0) shouldBe false

      bits(1) = true
      bits(32) = true
      bits(0) shouldBe false
      bits(1) shouldBe true
      bits(32) shouldBe true
      bits(63) shouldBe false

      bits(63) = true
      bits(63) shouldBe true
    }
  }

  describe("Exercise 08") {
    it("should access and update matrix cells") {
      val mat = Matrix(2, 3)
      mat(1, 1) shouldEqual 0
      mat(2, 3) shouldEqual 0

      mat(1, 1) = 3
      mat(2, 3) = -5
      mat(1, 1) shouldEqual 3
      mat(2, 3) shouldEqual -5
    }

    it("should parse matrix") {
      val mat = Matrix.parse(
        """
        4 3 2
       -2 5 5
        """)

      mat(1, 1) shouldEqual 4
      mat(2, 1) shouldEqual -2
      mat(2, 3) shouldEqual 5
    }

    it("should add two matrices") {
      val m1 = Matrix.parse(
        """
          1 3 1
          1 0 0
        """)
      val m2 = Matrix.parse(
        """
          0 0 5
          7 5 0
        """)

      m1 + m2 shouldEqual Matrix.parse(
        """
          1 3 6
          8 5 0
        """)

    }

    it("should multiply two matrices") {
      val m1 = Matrix.parse(
        """
          2 3 4
          1 0 0
        """)
      val m2 = Matrix.parse(
        """
          0 1000
          1  100
          0   10
        """)

      m1 * m2 shouldEqual Matrix.parse(
        """
          3 2340
          0 1000
        """)

    }

    it("should multiply matrix by scalar") {
      val m1 = Matrix.parse(
        """
          2 3 4
          1 0 0
        """)

      m1 * 2 shouldEqual Matrix.parse(
        """
          4 6 8
          2 0 0
        """)

    }
  }

  describe("Exercise 09") {
    def path(p: String): java.nio.file.Path = {
      new File(p).toPath
    }

    it("should unapply root path") {
      val PathComponents(dir, filename) = path("/foo")
      dir shouldEqual "/"
      filename shouldEqual "foo"
    }

    it("should unapply complex path") {
      val PathComponents(dir, filename) = path("/diggi/loo/diggi/ley")
      dir shouldEqual "/diggi/loo/diggi"
      filename shouldEqual "ley"
    }

    it("should fail if no filename") {
      assertThrows[MatchError] {
        val PathComponents(dir, filename) = path("/")
      }
    }
  }

  describe("Exercise 10") {
    def path(p: String): java.nio.file.Path = {
      new File(p).toPath
    }

    it("should match case with exact number of parts") {
      val p = path("/home/cay/readme.txt")
      p match {
        case SeqPathComponents(part1) => fail()
        case SeqPathComponents(part1, part2) => fail()
        case SeqPathComponents(part1, part2, part3) =>
          part1 shouldEqual "home"
          part2 shouldEqual "cay"
          part3 shouldEqual "readme.txt"
      }
    }

    it("should fail if no filename") {
      assertThrows[MatchError] {
        val SeqPathComponents(part1) = path("/")
      }
    }
  }

  describe("Exercise 11") {
    it("should dynamically select property path separated by periods") {
      val sysProps = new DynamicProps(System.getProperties)
      // I assure you, if this test fails on some environments, the problem most definitely is not in the test.
      sysProps.os.name.toString shouldEqual "Linux"
    }

    it("should dynamically update property path separated by periods") {
      val sysProps = new DynamicProps(System.getProperties)
      sysProps.user.nickname = "Kepponen"
      sysProps.user.nickname.toString shouldEqual "Kepponen"
    }
  }

  describe("Exercise 12") {
    val scalaXml =
      <html>
        <body id="foo">
          <h1>Moro!</h1>
          <ul id="11">
            <li id="mustamakkara">...</li>
          </ul>
          <ul id="42">
            <li id="riävä">...</li>
            <li id="holipompeli">...</li>
          </ul>
        </body>
      </html>

    val xml = XMLElement.fromScalaXml(scalaXml)

    it("should find element in a simple document") {
      val body = xml.html.body.iterator.next
      body.label shouldEqual "body"
      body.attr("id").get shouldEqual "foo"
    }

    it("should find elements with attribute filtering") {
      val liElems = xml.html.body.ul(id = "42").li
      val liIter = liElems.iterator

      liIter.next.attr("id").get shouldEqual "riävä"
      liIter.next.attr("id").get shouldEqual "holipompeli"
      liIter.hasNext shouldBe false
    }

    it("should find child elements of multiple elements") {
      val liElems = xml.html.body.ul.li
      val liIter = liElems.iterator

      liIter.next.attr("id").get shouldEqual "mustamakkara"
      liIter.next.attr("id").get shouldEqual "riävä"
      liIter.next.attr("id").get shouldEqual "holipompeli"
      liIter.hasNext shouldBe false
    }
  }

  describe("Exercise 13") {
    it("should build element") {
      val builder = new XMLBuilder()
      val ul = builder.ul(id = "42", style = "list-style: lower-alpha;")
      ul.label shouldEqual "ul"
      ul.attr("style").get shouldEqual "list-style: lower-alpha;"
    }

    it("should build nested elements conveniently") {
      val builder = new XMLBuilder()
      val ul =
        builder.ul(id = "42",
          builder.li(id = "12",
            builder.span),
          builder.li(id = "32")
        )

      ul.label shouldEqual "ul"
      ul.attr("id").get shouldEqual "42"

      ul.children.head.label shouldEqual "li"
      ul.children.head.attr("id").get shouldEqual "12"
      ul.children.head.children.head.label shouldEqual "span"
      ul.children(1).attr("id").get shouldEqual "32"
    }
  }
}