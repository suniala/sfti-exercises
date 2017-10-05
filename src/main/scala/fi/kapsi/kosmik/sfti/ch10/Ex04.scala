package fi.kapsi.kosmik.sfti.ch10

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
