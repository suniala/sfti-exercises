package fi.kapsi.kosmik.sfti

import java.io.InputStream

import scala.collection.{SortedMap, immutable, mutable}

object Chapter04 {

  /**
    * Set up a map of prices for a number of gizmos that you covet. Then produce
    * a second map with the same keys and the prices at a 10 percent discount.
    */
  object Ex01 {
    def discount(gizmos: Map[String, Double]): Map[String, Double] =
      gizmos.map({ case (name, price) => name -> price * 0.9 })
  }

  /**
    * Write a program that reads words from a file. Use a mutable map to count
    * how often each word appears... At the end, print out all words and their counts.
    * NOTE: I chose to use InputStream instead of File.
    */
  object Ex02 {
    def countWords(input: InputStream): Map[String, Int] = {
      val scanner = new java.util.Scanner(input)
      val occurrences = mutable.Map[String, Int]() withDefaultValue 0
      while (scanner.hasNext()) {
        val word = scanner.next()
        val count = occurrences(word)
        occurrences(word) = count + 1
      }

      occurrences.toMap
    }
  }

  /**
    * Repeat the preceding exercise with an immutable map.
    */
  object Ex03 {
    def countWords(input: InputStream): Map[String, Int] = {
      def countWords(acc: Map[String, Int], words: List[String]): Map[String, Int] = {
        if (words.isEmpty) acc
        else {
          val word = words.head
          val count = acc(word)
          countWords(acc + (word -> (count + 1)), words.tail)
        }
      }

      import scala.collection.JavaConverters._
      val scanner = new java.util.Scanner(input)
      countWords(Map() withDefaultValue 0, scanner.asScala.toList)
    }
  }

  /**
    * Repeat the preceding exercise with a sorted map, so that the words are
    * printed in sorted order.
    */
  object Ex04 {
    def countWords(input: InputStream): SortedMap[String, Int] = {
      def countWords(acc: SortedMap[String, Int], words: List[String]): SortedMap[String, Int] = {
        if (words.isEmpty) acc
        else {
          val word = words.head
          val count = if (acc.contains(word)) acc(word) else 0
          countWords(acc + (word -> (count + 1)), words.tail)
        }
      }

      import scala.collection.JavaConverters._
      val scanner = new java.util.Scanner(input)
      // NOTE: SortedMap does not support withDefaultValue
      countWords(SortedMap(), scanner.asScala.toList)
    }
  }

  /**
    * Repeat the preceding exercise with a java.util.TreeMap that you adapt to the
    * Scala API.
    */
  object Ex05 {
    def countWords(input: InputStream): mutable.Map[String, Int] = {
      import scala.collection.JavaConverters._
      val occurrences: mutable.Map[String, Int] = new java.util.TreeMap[String, Int]().asScala

      val scanner = new java.util.Scanner(input)
      while (scanner.hasNext()) {
        val word = scanner.next()
        val count = if (occurrences.contains(word)) occurrences(word) else 0
        occurrences(word) = count + 1
      }

      occurrences
    }
  }

  /**
    * Define a linked hash map that maps "Monday" to java.util.Calendar.MONDAY , and
    * similarly for the other weekdays. Demonstrate that the elements are visited
    * in insertion order.
    */
  object Ex06 {
    def toLinkedMap(entries: List[(String, Int)]): mutable.LinkedHashMap[String, Int] = {
      val map = mutable.LinkedHashMap[String, Int]()
      for ((key, value) <- entries)
        map(key) = value
      map
    }
  }

  /**
    * Print a table of all Java properties reported by the getProperties method of the
    * java.lang.System class
    */
  object Ex07 {
    def systemProps(printAlso: Boolean = false): String = {
      val properties = java.lang.System.getProperties
      import scala.collection.JavaConverters._
      val names = properties.stringPropertyNames().asScala.toList.sorted

      val maxLength = names
        .map(_.length)
        .sorted(Ordering.Int.reverse)
        .head

      val formatted = names.map(n => f"${n.padTo(maxLength, ' ')} | ${properties.getProperty(n)}%n").mkString("")
      if (printAlso) print(formatted)
      formatted
    }
  }

  /**
    * Write a function minmax(values: Array[Int]) that returns a pair containing the
    * smallest and the largest values in the array.
    */
  object Ex08 {
    def minmax(values: Array[Int]): (Int, Int) = (values.min, values.max)
  }

  /**
    * Write a function lteqgt(values: Array[Int], v: Int) that returns a triple containing
    * the counts of values less than v, equal to v, and greater than v.
    */
  object Ex09 {
    def lteqgt(values: Array[Int], v: Int): (Int, Int, Int) = {
      def count(acc: (Int, Int, Int), rem: Array[Int]): (Int, Int, Int) = {
        if (rem.isEmpty) acc
        else {
          if (rem.head < v) count(acc.copy(_1 = acc._1 + 1), rem.tail)
          else if (rem.head == v) count(acc.copy(_2 = acc._2 + 1), rem.tail)
          else count(acc.copy(_3 = acc._3 + 1), rem.tail)
        }
      }

      count((0, 0, 0), values)
    }
  }

  /**
    * What happens when you zip together two strings, such as "Hello".zip("World") ?
    * Come up with a plausible use case.
    */
  object Ex10 {
    def zip(a: String, b: String): immutable.IndexedSeq[(Char, Char)] = a.zip(b)
  }
}
