package fi.kapsi.kosmik.sfti

import java.util.concurrent.Executors

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object Chapter17 {

  /**
    * Consider the expression
    * <pre>
    * # for (n1 <- Future { Thread.sleep(1000) ; 2 }
    * #      n2 <- Future { Thread.sleep(1000); 40 })
    * #     println(n1 + n2)
    * </pre>
    * a) How is the expression translated to map and flatMap calls?
    * b) Are the two futures executed concurrently or one after the other?
    * c) In which thread does the call to println occur?
    */
  object Ex01 {

    import scala.concurrent.ExecutionContext.Implicits.global

    // a) This method answers the question
    def futureDemonstration(): Future[Int] = {
      // Note the use of def (instead of val) which ensures the futures will be started by f1.flatMap and f2.map, which
      // would be the same as using inline futures.
      def f1 = Future {
        Thread.sleep(1000)
        2
      }

      def f2 = Future {
        Thread.sleep(1000)
        40
      }

      // b) f1 is executed first and f2 after f1 has completed successfully
      // c) println call occurs in f2's thread
      f1.flatMap(n1 => f2.map(n2 => {
        println(n1 + n2)
        n1 + n2
      }))
    }
  }

  /**
    * Write a function doInOrder that, given two functions f: T => Future[U] and g: U
    * => Future[V] , produces a function T => Future[U] that, for a given t , eventually
    * yields g(f(t)) .
    *
    * NOTE: I assume there is a typo in the exercise and the expected function is T => Future[V]
    */
  object Ex02 {

    import scala.concurrent.ExecutionContext.Implicits.global

    def doInOrder[T, U, V](f: T => Future[U], g: U => Future[V]): T => Future[V] =
      (t: T) => f(t).flatMap(u => g(u))
  }

  /**
    * Repeat the preceding exercise for any sequence of functions of type T =>
    * Future[T] .
    */
  object Ex03 {

    import scala.concurrent.ExecutionContext.Implicits.global

    def doInOrder[T](fs: Seq[T => Future[T]]): T => Future[T] = {
      def evaluateInOrder[U](f: Future[U], rem: Seq[U => Future[U]]): Future[U] = {
        if (rem.isEmpty) f
        else f.flatMap(u => evaluateInOrder(rem.head(u), rem.tail))
      }

      if (fs.isEmpty) throw new IllegalArgumentException("need at least one function")
      (t: T) => evaluateInOrder(fs.head(t), fs.tail)
    }
  }

  /**
    * Write a function doTogether that, given two functions f: T => Future[U] and
    * g: U => Future[V] , produces a function T => Future[(U, V)] , running the two
    * computations in parallel and, for a given t , eventually yielding (f(t), g(t)) .
    *
    * NOTE: I assume there is a typo in the exercise and g should be T => Future[V].
    */
  object Ex04 {

    import scala.concurrent.ExecutionContext.Implicits.global

    def doTogether[T, U, V](f: T => Future[U], g: T => Future[V]): T => Future[(U, V)] = {
      (t: T) => {
        val fRes = f(t)
        val gRes = g(t)
        fRes.flatMap(u => gRes.map(v => (u, v)))
      }
    }
  }

  /**
    * Write a function that receives a sequence of futures and returns a future that
    * eventually yields a sequence of all results.
    */
  object Ex05 {

    import scala.concurrent.ExecutionContext.Implicits.global

    def reduceFutures[T](fs: Seq[Future[T]]): Future[Seq[T]] = Future.sequence(fs)
  }

  /**
    * Write a method Future[T] repeat(action: => T, until: T => Boolean)
    * that asynchronously repeats the action until it produces a value that is
    * accepted by the until predicate, which should also run asynchronously. Test
    * with a function that reads a password from the console, and a function that
    * simulates a validity check by sleeping for a second and then checking
    * that the password is "secret" . Hint: Use recursion.
    */
  object Ex06 {

    import scala.concurrent.ExecutionContext.Implicits.global

    def repeat[T](action: => T, until: T => Boolean): Future[T] = {
      @tailrec def doActionUntil(): T = {
        val result = action
        if (until(result)) result
        else doActionUntil()
      }

      Future[T] {
        doActionUntil()
      }
    }
  }

  /**
    * Write a program that counts the prime numbers between 1 and n, as reported
    * by BigInt.isProbablePrime . Divide the interval into p parts, where p is the number
    * of available processors. Count the primes in each part in concurrent futures
    * and combine the results.
    */
  object Ex07 {

    val pool = Executors.newCachedThreadPool()
    implicit val ec = ExecutionContext.fromExecutor(pool)
    //    import scala.concurrent.ExecutionContext.Implicits.global

    def primesSequential(upTo: Int): Future[Seq[Int]] =
      Future {
        for (i <- 1 to upTo if BigInt(i).isProbablePrime(10)) yield i
      }

    def primesConcurrentC(upTo: Int): Future[Seq[Int]] = {
      val processors = Runtime.getRuntime.availableProcessors()

      // FIXME: this may create partitions like (2, 6, 10, ...) or (4, 8, 12, ...) which are quite useless
      val partitionedComputations = (1 to processors)
        .map(partition => Future {
          println(f"start $partition")
          val res = (partition to upTo by processors)
            .filter(BigInt(_).isProbablePrime(10))
          println(f"end $partition")
          res
        })
      Future.sequence(partitionedComputations).map(s => s.flatten).map(s => s.sorted)
    }

    def primesCountConcurrent(upTo: Int): Future[Int] = {
      val partitions = Runtime.getRuntime.availableProcessors()

      def candidates(from: Int, upTo: Int, by: Int): Stream[Int] = {
        def it(from: Int, upTo: Int): Iterator[Int] = new Iterator[Int] {
          var curr: Int = 1

          override def hasNext: Boolean = curr < upTo

          override def next(): Int = {
            if (curr <= 2) curr += 1
            else curr += 2
            curr
          }
        }

        it(from, upTo)
          .toStream
          .zipWithIndex
          .filter({
            case (_, index) => (index + 1) >= from && (index + from - 1) % by == 0
          })
          .map({
            case (cand, _) => cand
          })
      }

      val partitionedComputations = (1 to partitions)
        .map(partition => Future {
          println(f"start $partition")
          val res = candidates(partition, upTo, partitions)
            .map(c => {
              if (c < 20) println(f"sample candidate $partition $c")
              c
            })
            .filter(BigInt(_).isProbablePrime(10))
            .count(_ => true)
          println(f"done $partition, count $res")
          res
        })
      Future.sequence(partitionedComputations).map(s => s.sum)
    }

    def primesCountConcurrentA(upTo: Int): Future[Int] = {
      val processors = Runtime.getRuntime.availableProcessors()

      val partitionedComputations = (1 to processors)
        .map(partition => Future {
          println(f"start $partition")
          val res = (partition to upTo by processors)
            .filter(BigInt(_).isProbablePrime(10))
            .count(_ => true)
          println(f"done $partition, count $res")
          res
        })
      Future.sequence(partitionedComputations).map(s => s.sum)
    }

    def primesConcurrentB(upTo: Int): Future[Seq[Int]] = {
      val processors = Runtime.getRuntime.availableProcessors()
      val partitionSize = (upTo.toDouble / processors).ceil.toInt
      val partitions = (1 to processors)

      val partitionedComputations = partitions.map(partition => Future {
        println(f"start $partition")
        val res = (1 to partitionSize)
          .map(index => (partition - 1) * partitionSize + index)
          .filter(_ <= upTo)
          .filter(BigInt(_).isProbablePrime(10))
        println(f"done $partition")
        res
      })
      Future.sequence(partitionedComputations).map(s => s.flatten)
    }

    def primesConcurrentA(upTo: Int): Future[Seq[Int]] = {
      val processors = Runtime.getRuntime.availableProcessors()
      val partitionSize = (upTo.toDouble / processors).ceil.toInt
      val partitions = (1 to processors)
        .map(partition => (1 to partitionSize)
          .map(index => (partition - 1) * partitionSize + index)
          .takeWhile(_ <= upTo))

      val partitionedComputations = partitions.map(partition => Future {
        println(f"start ${partition.head}")
        val res = partition.filter(BigInt(_).isProbablePrime(10))
        println(f"end ${partition.head}")
        res
      })
      Future.sequence(partitionedComputations).map(s => s.flatten)
    }
  }

}
