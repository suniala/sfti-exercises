package fi.kapsi.kosmik.sfti

import scala.concurrent.Future

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

}
