package fi.kapsi.kosmik.sfti

import java.net.URL
import java.util.Date
import java.util.concurrent.Executors

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.immutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.io.Source
import scala.util.{Failure, Success, Try}

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

    private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

    def primesSequential(upTo: Int): Future[Int] =
      Future {
        (for (i <- 1 to upTo if BigInt(i).isProbablePrime(10)) yield 1).sum
      }

    /**
      * Count primes concurrently so that thread t evaluates integers (t, t+threads, t+2*threads, ...), thread t+1
      * evaluates integers (t+1, t+1+threads, t+1+2*threads, ...) etc.
      * <p>
      * Note that some threads may get to evaluate sequences like (2, 6, 10, ...) which are quite useless to evaluate.
      *
      * @param upTo last integer to evaluate
      * @return number of probable primes found
      */
    def primesConcurrentTrivialSampling(upTo: Int): Future[Int] = {
      val processors = Runtime.getRuntime.availableProcessors()

      val partitionedComputations = (1 to processors)
        .map(partition => Future {
          (partition to upTo by processors)
            .filter(BigInt(_).isProbablePrime(10))
            .count(_ => true)
        })
      Future.sequence(partitionedComputations).map(s => s.sum)
    }

    /**
      * Count primes concurrently so that thread t evaluates integers (1, ..., upTo/threads), thread t+1 evaluates
      * integers (upTo/threads+1, ..., 2*upTo/threads) etc.
      *
      * @param upTo last integer to evaluate
      * @return number of probable primes found
      */
    def primesConcurrentRanges(upTo: Int): Future[Int] = {
      val partitions = Runtime.getRuntime.availableProcessors()
      val partitionSize = (upTo.toDouble / partitions).ceil.toInt

      val partitionedComputations = (1 to partitions)
        .map(partition => Future {
          (1 to partitionSize)
            .map(index => (partition - 1) * partitionSize + index)
            .takeWhile(_ <= upTo) // ensure the last partition does not exceed upTo
            .count(BigInt(_).isProbablePrime(10))
        })
      Future.sequence(partitionedComputations).map(s => s.sum)
    }
  }

  /**
    * Write a program that asks the user for a URL, reads the web page at that URL,
    * and displays all the hyperlinks. Use a separate Future for each of these three
    * steps.
    */
  object Ex08 {

    class Quit extends Exception

    private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

    def extractLinks(url: Option[String]): Future[List[String]] = {
      Future.successful(url)
        .flatMap({
          case Some(u) => Future.successful(Some(new URL(u)))
          case _ => acquireUrl()
        })
        .flatMap({
          case Some(u) => fetch(u)
          case _ => throw new Quit
        })
        .flatMap(doc => extract(doc))
    }

    private def extract(doc: String): Future[List[String]] =
      Future {
        val linkPattern = """href="(http[^"]+)"""".r
        linkPattern.findAllMatchIn(doc).map(m => m.group(1)).toList
      }

    private def fetch(url: URL): Future[String] = {
      println(f"Fetching $url")
      Future {
        Source.fromURL(url).mkString
      }
    }

    private def acquireUrl(): Future[Option[URL]] = Future {
      @tailrec def acquireInput(): Option[URL] = {
        print("Please enter url: ")
        val input = scala.io.StdIn.readLine()

        input match {
          case "" => None
          case stringInput: String =>
            val url = Try(new URL(stringInput))
            url match {
              case Success(u) => Some(u)
              case _ =>
                println("Invalid url!")
                acquireInput()
            }
        }
      }

      acquireInput()
    }
  }

  /**
    * Write a program that asks the user for a URL, reads the web page at that URL,
    * finds all the hyperlinks, visits each of them concurrently, and locates the Server
    * HTTP header for each of them. Finally, print a table of which servers were
    * found how often. The futures that visit each page should return the header.
    */
  object Ex09 {

    import Ex08.extractLinks

    private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

    def fetchLinkedHttpServerCounts(url: Option[String]): Future[Map[String, Int]] = {
      extractLinks(url)
        .flatMap(links => {
          val eventualServerNames = links
            .map(link => Future {
              toUrl(link)
                .map(url => fetchServerName(url)) // is not evaluated if toUrl returns Failure
                .map(serverName => serverName.get) // is not evaluated if fetchServerName returns None
            })

          Future.sequence(eventualServerNames)
            .map(possibleServerNames => possibleServerNames
              .filter(_.isSuccess) // we can't omit isSuccess as the collection under iteration is a List and not a Try
              .map(_.get)
              .foldLeft(immutable.Map[String, Int]() withDefaultValue 0)({
                (counts, serverName) => counts.updated(serverName, counts(serverName) + 1)
              }))
        })
    }

    private def fetchServerName(url: URL): Option[String] = {
      println(s"Fetching header for $url")
      val name = Option(url.openConnection().getHeaderField("Server"))
      if (name.isDefined) println(s"$url uses server: ${name.get}")
      else println(s"$url does not expose server name")
      name
    }

    private def toUrl(link: String): Try[URL] = Try(new URL(link))
  }

  /**
    * Change the preceding exercise where the futures that visit each header update
    * a shared Java ConcurrentHashMap or Scala TrieMap . This isn’t as easy as it sounds.
    * A threadsafe data structure is safe in the sense that you cannot corrupt its
    * implementation, but you have to make sure that sequences of reads and
    * updates are atomic.
    * <p>
    * NOTE: So as to make testing easier, I choose to make a simulated version of the preceding exercise.
    */
  object Ex10 {
    private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

    private class Counter {
      private val counts = TrieMap[String, Int]()

      def add(name: String): Unit = synchronized {
        val current = counts.getOrElse(name, 0)
        counts.put(name, current + 1)
      }

      def toMap: Map[String, Int] = counts.toMap
    }

    def fetchLinkedHttpServerCounts(fetchUrls: () => Future[List[URL]],
                                    fetchServerName: URL => Future[Option[String]]): Future[Map[String, Int]] = {
      val counts = new Counter

      // FIXME: This may lead to OutOfMemoryError with very large numbers of urls
      fetchUrls()
        .flatMap(links => {
          val eventualServerNames = links
            .map(link => {
              fetchServerName(link)
                .andThen({
                  case Success(Some(serverName)) => counts.add(serverName)
                })
            })
          Future.sequence(eventualServerNames)
        }).map(_ => counts.toMap)
    }
  }

  /**
    * Using futures, run four tasks that each sleep for ten seconds and then print
    * the current time. If you have a reasonably modern computer, it is very likely
    * that it reports four available processors to the JVM, and the futures should
    * all complete at around the same time. Now repeat with forty tasks. What
    * happens? Why? Replace the execution context with a cached thread pool.
    * What happens now? (Be careful to define the futures after replacing the implicit
    * execution context.)
    */
  object Ex11 {
    /**
      * As this implementation uses the default global execution context, it can only run as many concurrent threads
      * as there are available processors. If `threads` is larger that processor count, then some threads will print
      * time (and complete) only after others have already been completed.
      *
      * @param threads number of threads to run
      * @return a future unity
      */
    def delayedPrintTimeUsingDefaultEC(threads: Int): Future[Unit] = {
      runThreads(threads, scala.concurrent.ExecutionContext.Implicits.global)
    }

    /**
      * This implementation uses a thread pool execution context and should be able to run a large number of threads
      * concurrently. You can expect each thread to print (and complete) around the same time.
      *
      * @param threads number of threads to run
      * @return a future unity
      */
    def delayedPrintTimeUsingThreadPoolEC(threads: Int): Future[Unit] = {
      runThreads(threads, ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))
    }

    private def runThreads(threads: Int, executionContext: ExecutionContext): Future[Unit] = {
      implicit val ec: ExecutionContext = executionContext

      val fs = for (i <- 1 to threads) yield Future {
        println(s"  thread $i: start")
        Thread.sleep(1000)
        println(s"  thread $i: ${new Date}")
      }

      Future.sequence(fs).map(_ => Future {})
    }
  }

  /**
    * Write a method that, given a URL, locates all hyperlinks, makes a promise
    * for each of them, starts a task in which it will eventually fulfill all promises,
    * and returns a sequence of futures for the promises. Why would it not be a
    * good idea to return a sequence of promises?
    * <p>
    * Answer: returning a sequence of promises is not a good idea because promise is writable and thus, in practice,
    * exposes implementation internals to the caller.
    */
  object Ex12 {
    private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

    def simulateWithPromises(url: URL,
                             fetchLinks: URL => Future[List[String]],
                             processLink: String => String): List[Future[String]] = {
      // NOTE: This is a bit convoluted but the aim is to show what it takes to implement a function that has a
      // Future[List[T]] as input and needs to produce a List[Future[T]]. The only way I could figure out was
      // to synchronously wait for Future[List[T]] to complete.
      val linkPromises = waitForLinks(fetchLinks(url)).map(link => (link, Promise[String]()))

      Future[Unit] {
        linkPromises.foreach(ls => {
          Thread.sleep(10)
          ls._2.success(processLink(ls._1))
        })
      }

      linkPromises.map(ls => ls._2.future)
    }

    private def waitForLinks(eventualLinks: Future[List[String]]) = {
      Await.ready(eventualLinks, Duration.Inf)

      val links = eventualLinks.value.get match {
        case Success(res) => res
        case Failure(ex) => throw ex
      }
      links
    }
  }

  /**
    * Use a promise for implementing cancellation. Given a range of big integers,
    * split the range into subranges that you concurrently search for palindromic
    * primes. When such a prime is found, set it as the value of the future. All tasks
    * should periodically check whether the promise is completed, in which case
    * they should terminate.
    */
  object Ex13 {
    private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

    class WorkerStats {
      private val stats = new TrieMap[Int, Int]()

      def toMap: immutable.Map[Int, Int] = synchronized {
        stats.toMap
      }

      private[Ex13] def tick(worker: Int) = synchronized {
        stats.put(worker, stats.getOrElse(worker, 0) + 1)
      }
    }

    def findFirstPalindromicPrime(from: Int, upTo: Int, workers: Int = Runtime.getRuntime.availableProcessors()):
    Future[(Int, WorkerStats)] = {
      val stats = new WorkerStats
      val promise = Promise[(Int, WorkerStats)]()

      val partitionSize = ((upTo - from).toDouble / workers).ceil.toInt
      (1 to workers)
        .map(worker => Future {
          val cs = (1 to partitionSize)
            .map(index => (worker - 1) * partitionSize + from + index - 1)
            .takeWhile(_ <= upTo) // ensure the last partition does not exceed upTo
            .iterator

          while (!promise.isCompleted && cs.hasNext) {
            stats.tick(worker)
            val candidate = cs.next()
            if (BigInt(candidate).isProbablePrime(10) && candidate.toString == candidate.toString.reverse) {
              promise.tryComplete(Success((candidate, stats)))
            }
          }
        })

      promise.future
    }
  }

}