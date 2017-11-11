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

}
