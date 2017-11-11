package fi.kapsi.kosmik.sfti

object Chapter12 {

  /**
    * Write a function values(fun: (Int) => Int, low: Int, high: Int) that yields a collec-
    * tion of function inputs and outputs in a given range. For example, values(x =>
    * x * x, -5, 5) should produce a collection of pairs (-5, 25) , (-4, 16) , (-3, 9) , . . . ,
    * (5, 25) .
    */
  object Ex01 {
    def values(fun: (Int) => Int, low: Int, high: Int): List[(Int, Int)] = {
      (low to high map (i => (i, fun(i)))).toList
    }
  }

  /**
    * How do you get the largest element of an array with reduceLeft ?
    */
  object Ex02 {
    def largest(arr: Array[Int]): Int = arr.reduceLeft((a, b) => if (a > b) a else b)
  }

  /**
    * Implement the factorial function using to and reduceLeft , without a loop or
    * recursion.
    */
  object Ex03 {
    def factReduce(n: Int): Int = {
      if (n > 0) 1 to n reduceLeft (_ * _)
      else if (n == 0) 1
      else throw new IllegalArgumentException
    }
  }

  /**
    * The previous implementation needed a special case when n < 1. Show how
    * you can avoid this with foldLeft . (Look at the Scaladoc for foldLeft . It’s like
    * reduceLeft , except that the first value in the chain of combined values is supplied
    * in the call.)
    */
  object Ex04 {
    /**
      * Implementation note: Parentheses are required here. Otherwise the compiler will think we are trying to call
      * Int(1) with parameters.
      */
    def factFold(n: Int): Int = (1 to n foldLeft 1) (_ * _)
  }

  /**
    * Write a function largest(fun: (Int) => Int, inputs: Seq[Int]) that yields the largest
    * value of a function within a given sequence of inputs. For example, largest(x
    * => 10 * x - x * x, 1 to 10) should return 25 . Don’t use a loop or recursion.
    **/
  object Ex05 {
    def largest(fun: (Int) => Int, inputs: Seq[Int]): Int =
      inputs.map(fun).reduceLeft((a, b) => if (a > b) a else b)
  }

  /**
    * Modify the previous function to return the input at which the output is largest.
    * For example, largestAt(x => 10 * x - x * x, 1 to 10) should return 5 . Don’t use
    * a loop or recursion.
    **/
  object Ex06 {
    def largestAt(fun: (Int) => Int, inputs: Seq[Int]): Int =
      inputs
        .map(n => (n, fun(n)))
        .foldLeft((Int.MinValue, Int.MinValue))({ case ((ai, ao), (bi, bo)) => if (ao > bo) (ai, ao) else (bi, bo) })
        ._1
  }

  /**
    * It’s easy to get a sequence of pairs, for example:
    * <pre>
    * val pairs = (1 to 10) zip (11 to 20)
    * </pre>
    * Now, suppose you want to do something with such a sequence—say, add
    * up the values. But you can’t do
    * <pre>
    * pairs.map(_ + _)
    * </pre>
    * The function _ + _ takes two Int parameters, not an (Int, Int) pair. Write a
    * function adjustToPair that receives a function of type (Int, Int) => Int and returns
    * the equivalent function that operates on a pair. For example, adjustToPair(_ *
    * _)((6, 7)) is 42 .
    * Then use this function in conjunction with map to compute the sums of the
    * elements in pairs .
    */
  object Ex07 {
    def adjustToPair(fun: (Int, Int) => Int): ((Int, Int)) => Int = {
      case (a, b) => fun(a, b)
    }
  }

  /**
    * In Section 12.8, “Currying,” on page 164, you saw the corresponds method used
    * with two arrays of strings. Make a call to corresponds that checks whether the
    * elements in an array of strings have the lengths given in an array of integers.
    */
  object Ex08 {
    def correspondingLengths(s: Array[String], i: Array[Int]): Boolean = s.corresponds(i)(_.length == _)
  }

  /**
    * Implement corresponds without currying. Then try the call from the preceding
    * exercise. What problem do you encounter?
    **/
  object Ex09 {

    class Corresponder(val xs: Array[String]) {
      def corresponds[B](that: Array[B], p: (String, B) => Boolean): Boolean =
        xs.corresponds(that)(p)
    }

  }

  /**
    * Implement an unless control abstraction that works just like if , but with an
    * inverted condition. Does the first parameter need to be a call-by-name
    * parameter? Do you need currying?
    */
  object Ex10 {
    def callByValueUnless(condition: () => Boolean)(block: => Unit) {
      if (!condition()) {
        block
      }
    }

    def callByNameUnless(condition: => Boolean)(block: => Unit) {
      if (!condition) {
        block
      }
    }

    def nonCurryingUnless(condition: => Boolean, block: => Unit) {
      if (!condition) {
        block
      }
    }
  }

}
