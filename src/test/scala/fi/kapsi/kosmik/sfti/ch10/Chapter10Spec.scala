package fi.kapsi.kosmik.sfti.ch10

import java.beans.{PropertyChangeEvent, PropertyChangeListener}

import fi.kapsi.kosmik.sfti.ch10.Ex01.RectangleLike
import fi.kapsi.kosmik.sfti.ch10.Ex02.OrderedPoint
import fi.kapsi.kosmik.sfti.ch10.Ex04.{BufferLogger, CryptoLogger}
import fi.kapsi.kosmik.sfti.ch10.Ex05.PointBean
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSpec, Matchers}

class Chapter10Spec extends FunSpec with Matchers with MockFactory {
  describe("Exercise 01") {
    it("should mix translate and grow to Ellipse2D.Double") {
      val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
      egg.getCenterX shouldEqual 15
      egg.getCenterY shouldEqual 25
      egg.getWidth shouldEqual 20
      egg.getHeight shouldEqual 30

      egg.translate(10, -10)
      egg.getCenterX shouldEqual 25
      egg.getCenterY shouldEqual 15

      egg.grow(10, 20)
      egg.getCenterX shouldEqual 25
      egg.getCenterY shouldEqual 15
      egg.getWidth shouldEqual 40
      egg.getHeight shouldEqual 70
    }
  }

  describe("Exercise 02") {
    def orderedPoint(x: Int, y: Int): OrderedPoint = {
      val point = new OrderedPoint
      point.setLocation(x, y)
      point
    }

    // NOTE: Composing custom matchers is from: http://www.scalatest.org/user_guide/using_matchers#composingMatchers
    def composeCompare(res: Int, op2: OrderedPoint) = be(res) compose { (op1: OrderedPoint) => op1.compare(op2) }

    def orderBefore(op2: OrderedPoint) = composeCompare(-1, op2)

    def orderAt(op2: OrderedPoint) = composeCompare(0, op2)

    def orderAfter(op2: OrderedPoint) = composeCompare(1, op2)

    it("should mix ordering into point") {
      orderedPoint(1, 4) should orderBefore(orderedPoint(2, 3))
      orderedPoint(1, 4) should orderBefore(orderedPoint(1, 5))

      orderedPoint(1, 4) should orderAt(orderedPoint(1, 4))

      orderedPoint(1, 4) should orderAfter(orderedPoint(1, 3))
      orderedPoint(1, 4) should orderAfter(orderedPoint(0, 5))
    }
  }

  describe("Exercise 04") {
    it("should apply Caesar cipher with default key 3") {
      class Log(val buffer: StringBuffer) extends BufferLogger with CryptoLogger

      val log = new Log(new StringBuffer())
      log.buffer.toString shouldEqual ""

      log.log("It is full of stars")
      log.buffer.toString shouldEqual "Lw lv ixoo ri vwduv"
    }

    it("should apply Caesar cipher with given key") {
      class Log(val buffer: StringBuffer) extends BufferLogger with CryptoLogger {
        override def key: Int = -3
      }

      val log = new Log(new StringBuffer())
      log.log("Diggiloo diggiley")
      log.buffer.toString shouldEqual "Afddfill afddfibv"
    }
  }

  describe("Exercise 05") {
    it("should fire change listener via trait") {
      val locationListener = mock[PropertyChangeListener]
      (locationListener.propertyChange _).expects(where {
        (e: PropertyChangeEvent) => e.getPropertyName == "location" && e.getNewValue == new java.awt.Point(2, 4)
      }).once

      val point = new PointBean
      point.addPropertyChangeListener("location", locationListener)

      point.setLocation(2, 4)
    }
  }

}
