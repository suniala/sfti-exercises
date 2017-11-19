package fi.kapsi.kosmik.sfti

import scala.annotation.varargs

object Chapter15 {

  /**
    * Make an example class that shows every possible position of an annotation.
    * Use @deprecated as your sample annotation.
    */
  object Ex02 {

    @deprecated
    class Example {
      @deprecated
      var state: Int = 0

      @deprecated
      def foo(): Unit = println("Foo I tell you!")

      def fie(@deprecated times: Int): Unit = println("fie" * times)
    }

  }

  /**
    * Write a Scala method sum with variable integer arguments that returns the
    * sum of its arguments. Call it from Java.
    */
  // NOTE: methods of nested objects do not seem to be usable from Java code. Hence there is no Ex04 object here.
  @varargs
  def ex04_sum(numbers: Int*): Int = numbers.sum
}
