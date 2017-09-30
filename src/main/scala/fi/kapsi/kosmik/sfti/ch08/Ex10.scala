package fi.kapsi.kosmik.sfti.ch08

import scala.collection.immutable.Stack

/**
  * The file scala/collection/immutable/Stack.scala contains the definition
  * class Stack[A] protected (protected val elems: List[A])
  * Explain the meanings of the protected keywords.
  */
//noinspection ScalaDeprecation
object Ex10 {

  class IntStack(initial: List[Int]) extends Stack[Int](initial) {
    def peek(): Int = {
      // Subclasses have access to the protected elems field.
      elems.head
    }
  }

  val intStack = new IntStack(List(1, 2, 3))

  // Protected elems field can only be accessed from subclasses. The following would not compile:
  // val initialInt = intStack.elems.head

  // Stack constructor can only be invoked by subclasses. The following would not compile:
  // val stack = new Stack[Int](List(1, 2, 3))
}
