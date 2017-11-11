package fi.kapsi.kosmik.sfti

object Chapter10 {

  /**
    * The java.awt.Rectangle class has useful methods translate and grow that are un-
    * fortunately absent from classes such as java.awt.geom.Ellipse2D . In Scala, you
    * can fix this problem. Define a trait RectangleLike with concrete methods translate
    * and grow . Provide any abstract methods that you need for the implementation,
    * so that you can mix in the trait like this:
    * <pre>
    * val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
    * egg.translate(10, -10)
    * egg.grow(10, 20)
    * </pre>
    */
  object Ex01 {

    trait RectangleLike {
      def translate(dx: Int, dy: Int): Unit = {
        setFrame(getX + dx, getY + dy, getWidth, getHeight)
      }

      def grow(h: Int, v: Int): Unit = {
        setFrame(getX - h, getY - v, getWidth + 2 * h, getHeight + 2 * v)
      }

      def setFrame(x: Double, y: Double, w: Double, h: Double)

      def getX: Double

      def getY: Double

      def getWidth: Double

      def getHeight: Double
    }

  }

  /**
    * Define a class OrderedPoint by mixing scala.math.Ordered[Point] into java.awt.Point .
    * Use lexicographic ordering, i.e. (x, y) < (x’, y’) if x < x’ or x = x’ and y < y’.
    */
  object Ex02 {

    import java.awt.Point

    class OrderedPoint extends java.awt.Point with scala.math.Ordered[java.awt.Point] {
      override def compare(that: Point): Int = {
        if (getX < that.getX) -1
        else if (getX == that.getX) {
          if (getY < that.getY) -1
          else if (getY == that.getY) 0
          else 1
        }
        else 1
      }
    }

  }

  /**
    * Provide a CryptoLogger trait that encrypts the log messages with the Caesar ci-
    * pher. The key should be 3 by default, but it should be overridable by the user.
    * Provide usage examples with the default key and a key of –3.
    */
  object Ex04 {

    trait Logger {
      def log(msg: String)
    }

    trait BufferLogger extends Logger {
      def log(msg: String): Unit = {
        buffer.append(msg)
      }

      def buffer: StringBuffer
    }

    trait CryptoLogger extends Logger {
      // In Scala, this method override is still considered to be abstract as it still requires a
      // concrete log method (super.log(...)) to be mixed in. Hence "abstract" and "override".
      abstract override def log(msg: String): Unit = {
        // NOTE: We don't even try to handle going beyond the range of letters.
        super.log(msg.map((c: Char) => if (c.isLetter) (c + key).toChar else c).mkString(""))
      }

      def key: Int = 3
    }

  }

  /**
    * The JavaBeans specification has the notion of a property change listener, a
    * standardized way for beans to communicate changes in their properties. The
    * PropertyChangeSupport class is provided as a convenience superclass for any bean
    * that wishes to support property change listeners. Unfortunately, a class that
    * already has another superclass—such as JComponent —must reimplement
    * the methods. Reimplement PropertyChangeSupport as a trait, and mix it into the
    * java.awt.Point class.
    */
  object Ex05 {

    import java.beans.PropertyChangeListener

    /**
      * NOTE: Provides only a small subset (as an example) of the features of java.beans.PropertyChangeSupport.
      */
    trait PropertyChangeSupport {
      private val pcs = new java.beans.PropertyChangeSupport(this)

      def addPropertyChangeListener(propertyName: String, listener: PropertyChangeListener): Unit = {
        pcs.addPropertyChangeListener(propertyName, listener)
      }

      def firePropertyChange(propertyName: String, oldValue: Any, newValue: Any): Unit = {
        pcs.firePropertyChange(propertyName, oldValue, newValue)
      }
    }


    class PointBean extends java.awt.Point with PropertyChangeSupport {
      // As an example, add support for property changes for one property
      override def setLocation(x: Int, y: Int): Unit = {
        val oldValue = getLocation
        super.setLocation(x, y)
        firePropertyChange("location", oldValue, getLocation)
      }
    }

  }

  /**
    * In the java.io library, you add buffering to an input stream with a
    * BufferedInputStream decorator. Reimplement buffering as a trait. For simplicity,
    * override the read method.
    */
  object Ex09 {

    import scala.collection.mutable

    trait InputStream {
      def read(): Option[Char]
    }

    trait StringInputStream extends InputStream {
      val input: String

      private var index = 0

      override def read(): Option[Char] = {
        if (index < input.length) {
          val next = input.charAt(index)
          index += 1
          Option(next)
        } else {
          Option.empty
        }
      }
    }

    trait CallCountingInputStream extends InputStream {
      private var count = 0

      abstract override def read(): Option[Char] = {
        count += 1
        super.read()
      }

      def callCount: Int = count
    }

    trait BufferedInputStream extends InputStream {
      private val size = 4

      private val buffer = mutable.Stack[Char]()

      private var finished = false

      abstract override def read(): Option[Char] = {
        if (!finished && buffer.isEmpty) {
          for (i <- 1 to size) {
            // NOTE: Super naive handling of "EOF".
            if (!finished) {
              val next = super.read()
              next match {
                case Some(char) => buffer.push(char)
                case None => finished = true
              }
            }
          }
        }

        if (buffer.nonEmpty) {
          Option(buffer.pop())
        } else {
          Option.empty
        }
      }
    }

  }

  /**
    * Implement a class IterableInputStream that extends java.io.InputStream with the
    * trait Iterable[Byte] .
    */
  object Ex11 {

    abstract class IterableInputStream extends java.io.InputStream with Iterable[Byte] {
      override def read(): Int

      override def iterator: Iterator[Byte] = {
        new Iterator[Byte] {
          private var nextValue: Option[Byte] = Option.empty

          override def hasNext: Boolean = {
            nextValue match {
              case None => readNext()
              case _ =>
            }

            nextValue.isDefined
          }

          override def next(): Byte = {
            nextValue match {
              case None => readNext()
              case _ =>
            }

            nextValue match {
              case Some(value) => {
                nextValue = Option.empty
                value
              }
              case _ => throw new IllegalStateException("no elements left")

            }
          }

          private def readNext(): Unit = {
            val value = read()
            if (value == -1) {
              nextValue = Option.empty
            } else {
              nextValue = Option(value.toByte)
            }
          }
        }
      }
    }

    class IterableByteInputStream(private val input: Array[Byte]) extends IterableInputStream {
      private var index: Int = 0

      override def read(): Int = {
        if (index < input.length) {
          val value = input(index)
          index += 1
          value
        } else {
          -1
        }
      }
    }

  }

}