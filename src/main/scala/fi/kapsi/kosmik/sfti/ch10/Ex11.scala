package fi.kapsi.kosmik.sfti.ch10

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
