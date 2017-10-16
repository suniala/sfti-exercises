package fi.kapsi.kosmik.sfti.ch10

import scala.collection.mutable

/**
  * In the java.io library, you add buffering to an input stream with a
  * BufferedInputStream decorator. Reimplement buffering as a trait. For simplicity,
  * override the read method.
  */
object Ex09 {

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
