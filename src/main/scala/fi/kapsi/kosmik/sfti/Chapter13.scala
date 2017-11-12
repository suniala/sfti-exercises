package fi.kapsi.kosmik.sfti

import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}

object Chapter13 {

  /**
    * Write a function that, given a string, produces a map of the indexes of all
    * characters. For example, indexes("Mississippi") should return a map associating
    * 'M' with the set {0} , 'i' with the set {1, 4, 7, 10} , and so on. Use a mutable map
    * of characters to mutable sets. How can you ensure that the set is sorted?
    */
  object Ex01 {
    type Indexes = mutable.Map[Char, mutable.SortedSet[Int]]

    def indexes(str: String): Indexes = {
      val map: Indexes = mutable.Map() withDefault { _ => mutable.SortedSet() }

      str
        .zip(0 until str.length)
        .foreach({ case (char, index) => map(char) = map(char) + index })

      map
    }
  }

  /**
    * Repeat the preceding exercise, using an immutable map of characters to lists.
    */
  object Ex02 {
    type Indexes = immutable.Map[Char, immutable.SortedSet[Int]]

    def indexes(str: String): Indexes = {
      def rec(acc: Indexes, rem: String, index: Int): Indexes = {
        if (rem.isEmpty) acc
        else rec(acc + (rem.head -> (acc(rem.head) + index)), rem.tail, index + 1)
      }

      rec(immutable.Map() withDefault { _ => immutable.SortedSet() }, str, 0)
    }
  }

  /**
    * Write a function that removes every second element from a ListBuffer . Try it
    * two ways. Call remove(i) for all even i starting at the end of the list. Copy every
    * second element to a new list. Compare the performance.
    */
  object Ex03 {
    def removeEverySecondInPlace(list: ListBuffer[Int]): ListBuffer[Int] = {
      for (i <- (list.length - 1) to 0 by -2) {
        list.remove(i)
      }

      list
    }

    def removeEverySecondCopy(list: ListBuffer[Int]): ListBuffer[Int] = list.filter(_ % 2 == 1)
  }

  /**
    * Write a function that receives a collection of strings and a map from strings
    * to integers. Return a collection of integers that are values of the map corre-
    * sponding to one of the strings in the collection. For example, given Array("Tom",
    * "Fred", "Harry") and Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5) , return Array(3, 5) .
    * Hint: Use flatMap to combine the Option values returned by get .
    */
  object Ex04 {
    def lookup(keys: Array[String], map: Map[String, Int]): Array[Int] = keys.flatMap(key => map.get(key))
  }

  /**
    * Implement a function that works just like mkString , using reduceLeft .
    */
  object Ex05 {
    def mkString(values: Iterable[String], separator: String): String = values.reduceLeft((a, b) => a + separator + b)
  }

  /**
    * Given a list of integers lst , what is (lst :\ List[Int]())(_ :: _) ? (List[Int]() /:
    * lst)(_ :+ _) ? How can you modify one of them to reverse the list?
    */
  object Ex06 {
    def copyByFoldRight(lst: List[Int]): List[Int] = (lst :\ List[Int]()) (_ :: _)

    def copyByFoldLeft(lst: List[Int]): List[Int] = (List[Int]() /: lst) (_ :+ _)

    def reverseByFoldRight(lst: List[Int]): List[Int] = (lst :\ List[Int]()) ((a, b) => b :+ a)

    def reverseByFoldLeft(lst: List[Int]): List[Int] = (List[Int]() /: lst) ((a, b) => b :: a)
  }

  /**
    * In Section 13.10, “Zipping,” on page 187, the expression (prices zip quantities)
    * map { p => p._1 * p._2 } is a bit inelegant. We can’t do (prices zip quantities) map
    * { _ * _ } because _ * _ is a function with two arguments, and we need a
    * function with one argument that is a tuple. The tupled method of the Function
    * object changes a function with two arguments to one that takes a tuple. Apply
    * tupled to the multiplication function so you can map it over the list of pairs.
    */
  object Ex07 {
    def multiplyTuplesInelegant(prices: List[Double], quantities: List[Int]): List[Double] =
      (prices zip quantities) map { p => p._1 * p._2 }

    /**
      * NOTE: {...}.tupled does not seem to work with an anonymous wildcard functions. Maybe the wildcard types can
      * not be inferred?
      * <pre>
      * (prices zip quantities).map({
      * _ * _ // Cannot resolve symbol *
      * }.tupled)
      * <pre>
      *
      */
    def multiplyTuplesTupledWildcards(prices: List[Double], quantities: List[Int]): List[Double] =
      (prices zip quantities) map {
        Function.tupled(_ * _)
      }

    def multiplyTuplesTupledUtilityFunction(prices: List[Double], quantities: List[Int]): List[Double] = {
      val mul = (p: Double, q: Int) => p * q
      (prices zip quantities).map(mul.tupled)
    }

    def multiplyTuplesZipped(prices: List[Double], quantities: List[Int]): List[Double] = {
      // (prices, quantities).zipped is of type Tuple2Zipped[Double, List[Double], Int, List[Int]]
      // map is of type ((Double, Int) => B) => List[Double]
      (prices, quantities).zipped map {
        _ * _
      }
    }

    def multiplyTuplesCase(prices: List[Double], quantities: List[Int]): List[Double] =
      (prices zip quantities) map {
        case (p, q) => p * q
      }
  }

  /**
    * Write a function that turns an array of Double values into a two-dimensional
    * array. Pass the number of columns as a parameter. For example, with Array(1,
    * 2, 3, 4, 5, 6) and three columns, return Array(Array(1, 2, 3), Array(4, 5, 6)) .
    * Use the grouped method.
    */
  object Ex08 {
    def wrap(values: Array[Double], columns: Int): Array[Array[Double]] = values.grouped(columns).toArray
  }

  /**
    * The Scala compiler transforms a for / yield expression
    * <pre>
    * for (i <- 1 to 10; j <- 1 to i) yield i * j
    * </pre>
    * to invocations of flatMap and map , like this:
    * <pre>
    * (1 to 10).flatMap(i => (1 to i).map(j => i * j))
    * </pre>
    * Explain the use of flatMap . Hint: What is (1 to i).map(j => i * j) when i is 1 , 2 , 3 ?
    * What happens when there are three generators in the for / yield expression?
    */
  object Ex09 {
    type Result = IndexedSeq[Int]

    def twoIterablesFor(itemsA: Int, itemsB: Int): Result =
      for (i <- 1 to itemsA; j <- 1 to itemsB) yield i * j

    def twoIterablesMap(itemsA: Int, itemsB: Int): Result = {
      // flatMap is needed here because (1 to i).map(...) produces collections from which we need to collect elements
      // into a "flat" collection.
      (1 to itemsA)
        .flatMap(i => (1 to itemsB)
          .map(j => i * j)
        )
    }

    def threeIterablesFor(itemsA: Int, itemsB: Int, itemsC: Int): Result =
      for (i <- 1 to itemsA; j <- 1 to itemsB; k <- 1 to itemsC) yield i * j * k

    def threeIterablesMap(itemsA: Int, itemsB: Int, itemsC: Int): Result =
      (1 to itemsA)
        .flatMap(i => (1 to itemsB)
          .flatMap(j => (1 to itemsC)
            .map(k => i * j * k)
          )
        )
  }

  /**
    * The method java.util.TimeZone.getAvailableIDs yields time zones such as Africa/
    * Cairo and Asia/Chungking . Which continent has the most time zones? Hint: groupBy .
    */
  object Ex10 {
    def continentWithMostTimeZones(): String = {
      java.util.TimeZone.getAvailableIDs
        .filter(_.contains("/"))
        .groupBy(tz => tz.takeWhile(_ != '/'))
        .toList
        // alternatively: .sortWith(_._2.length < _._2.length)
        .sortWith({ case ((_, at), (_, bt)) => at.length < bt.length })
        .head
        ._1
    }
  }

  /**
    * Harry Hacker reads a file into a string and wants to use a parallel collection
    * to update the letter frequencies concurrently on portions of the string. He
    * uses the following code:
    * val frequencies = new scala.collection.mutable.HashMap[Char, Int]
    * for (c <- str.par) frequencies(c) = frequencies.getOrElse(c, 0) + 1
    * Why is this a terrible idea? How can he really parallelize the computation?
    * (Hint: Use aggregate .)
    */
  object Ex11 {
    def frequenciesSingleThread(str: String): collection.Map[Char, Int] = {
      def frequencies(acc: immutable.Map[Char, Int], rem: String): immutable.Map[Char, Int] = {
        if (rem.isEmpty) acc
        else frequencies(acc + (rem.head -> (acc.getOrElse(rem.head, 0) + 1)), rem.tail)
      }

      frequencies(immutable.Map(), str)
    }

    /**
      * DO NOT USE: this implementation produces invalid results!
      */
    def frequenciesHackerParallel(str: String): collection.Map[Char, Int] = {
      val frequencies = new scala.collection.mutable.HashMap[Char, Int]
      for (c <- str.par) frequencies(c) = frequencies.getOrElse(c, 0) + 1
      frequencies
    }

    def frequenciesFixedParallel(str: String): collection.Map[Char, Int] = {
      str.par.aggregate(
        immutable.HashMap[Char, Int]()
      )(
        (fs, c) => fs + (c -> (fs.getOrElse(c, 0) + 1)),
        _.merged(_)({ case ((ak, av), (_, bv)) => (ak, av + bv) })
      )
    }
  }

}
