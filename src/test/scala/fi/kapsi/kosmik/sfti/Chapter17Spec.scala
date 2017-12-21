package fi.kapsi.kosmik.sfti

import java.net.URL
import java.time.Duration
import java.util.concurrent.Executors

import fi.kapsi.kosmik.sfti.test.StopWatch
import fi.kapsi.kosmik.sfti.{Chapter17 => chapter}
import org.scalatest.{AsyncFunSpec, Matchers}

import scala.collection.{immutable, mutable}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

class Chapter17Spec extends AsyncFunSpec with Matchers with ExerciseSupport {
  /**
    * Use this execution context for code that needs to run concurrently. The implicit execution context provided
    * by AsyncFunSpec confines execution to a single thread and should not be used if you, for example, define
    * asynchronous functions in test code and expect the production code to run these concurrently.
    */
  private val concurrentExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  describe("Exercise 01") {
    import chapter.Ex01._

    it("should run futureDemonstration") {
      val futureSum = futureDemonstration()
      futureSum map { sum => sum shouldEqual 42 }
    }
  }

  describe("Exercise 02") {
    import chapter.Ex02._

    it("should do g(f(t)) when g and f are functions that produce one Future each") {
      def f = (t: Double) => Future[Int] {
        Thread.sleep(18)
        t.floor.toInt
      }

      def g = (u: Int) => Future[String] {
        Thread.sleep(11)
        "#" * u
      }

      doInOrder(f, g)(3.2) map { v => v shouldEqual "###" }
    }
  }

  describe("Exercise 03") {
    import chapter.Ex03._

    it("should do fn(...(f1(f0(t)))) when f is a function that produces a Future") {
      // t * 2 - 3 / 3 + 1
      val fs = Seq[Int => Future[Int]](
        (t: Int) => Future {
          Thread.sleep(2000)
          t * 2
        },
        (t: Int) => Future {
          Thread.sleep(500)
          t - 3
        },
        (t: Int) => Future {
          Thread.sleep(100)
          t / 3
        },
        (t: Int) => Future {
          Thread.sleep(1000)
          t + 1
        }
      )

      // Use assertDurationLess to verify doInOrder is non-blocking.
      val eventualInt = assertDurationLess(Duration.ofMillis(100)) {
        doInOrder(fs)(6)
      }
      eventualInt map { v => v shouldEqual 4 }
    }
  }

  describe("Exercise 04") {
    import chapter.Ex04._

    it("should yield (f(t), g(t)) by running these asynchronous functions concurrently") {
      val f = (t: Int) => Future[Int] {
        Thread.sleep(2000)
        ("1" * t).toInt
      }(concurrentExecutionContext)

      val g = (t: Int) => Future[String] {
        Thread.sleep(500)
        "x" * t
      }(concurrentExecutionContext)

      val eventualResult = assertDurationLess(Duration.ofMillis(100)) {
        doTogether(f, g)(5)
      }
      eventualResult map {
        case (aRes, bRes) =>
          aRes shouldEqual 11111
          bRes shouldEqual "xxxxx"
      }
    }
  }

  describe("Exercise 05") {
    import chapter.Ex05._

    it("should yield Future of a sequence") {
      val results = 1 to 5
      val durations = for (i <- results) yield (results.last + 1 - i) * 100
      val fs: Seq[Future[Int]] = results
        .zip(durations)
        .map(
          { case (result, duration) =>
            Future {
              Thread.sleep(duration)
              result
            }(concurrentExecutionContext)
          })

      val eventualResult = assertDurationLess(Duration.ofMillis(100)) {
        reduceFutures(fs)
      }

      val stopWatch = StopWatch.start
      eventualResult map {
        xs => {
          assert(stopWatch.split < Duration.ofMillis((durations.max * 1.1).toInt),
            "futures should be evaluated in parallel in about " + durations.max + " millis")
          xs shouldEqual results
        }
      }
    }
  }

  describe("Exercise 06") {
    import chapter.Ex06._

    it("should repeat action asynchronously until condition is met") {
      val remainingInputs = mutable.Stack[String]("passw0rd", "pwd", "foo", "secret", "fie")

      def action = {
        Thread.sleep(200)
        remainingInputs.pop()
      }

      val pastInputs = mutable.Stack[String]()
      val validate = (input: String) => {
        Thread.sleep(100)
        pastInputs.push(input)
        input == "secret"
      }

      val eventualResult = assertDurationLess(Duration.ofMillis(100)) {
        repeat(action, validate)
      }

      eventualResult map {
        validInput => {
          remainingInputs.length shouldEqual 1
          pastInputs.length shouldEqual 4
          validInput shouldEqual "secret"
        }
      }
    }
  }

  describe("Exercise 07") {
    import chapter.Ex07._

    val performanceTestN = 5000000
    val performanceTestExpectedPrimes = 348513

    it("should count small number of primes sequentially") {
      primesSequential(70) map {
        count => count shouldEqual 19
      }
    }

    it("should count large number of primes sequentially") {
      val stopWatch = StopWatch.start

      primesSequential(performanceTestN) map {
        count => {
          val split = stopWatch.split()
          info(f"counting $count primes sequentially took $split")
          count shouldEqual performanceTestExpectedPrimes
        }
      }
    }

    it("should count small number of primes concurrently with primesConcurrentTrivialSampling") {
      primesConcurrentTrivialSampling(70) map {
        size => size shouldEqual 19
      }
    }

    it("should count large number of primes with primesConcurrentTrivialSampling") {
      val stopWatch = StopWatch.start

      primesConcurrentTrivialSampling(performanceTestN) map {
        count => {
          val split = stopWatch.split()
          info(f"counting $count primes with primesConcurrentTrivialSampling took $split")
          count shouldEqual performanceTestExpectedPrimes
        }
      }
    }

    it("should count large number of primes with primesConcurrentRanges") {
      val stopWatch = StopWatch.start

      primesConcurrentRanges(performanceTestN) map {
        count => {
          val split = stopWatch.split()
          info(f"counting $count primes with primesConcurrentRanges took $split")
          count shouldEqual performanceTestExpectedPrimes
        }
      }
    }
  }

  describe("Exercise 10") {
    import chapter.Ex10._

    def expectedCounts(serverNames: Map[_, Option[String]]): Map[String, Int] = {
      val counts = serverNames.foldLeft(Map[String, Int]() withDefaultValue 0)({
        case (acc, (_, Some(name))) => acc.updated(name, acc(name) + 1)
        case (acc, _) => acc
      })
      counts
    }

    it("should count small number server names") {
      val serverNames = Map[String, Option[String]](
        "http://a.org" -> Some("Apache"),
        "http://b.org" -> Some("nginx"),
        "http://c.org" -> Some("Apache/2.4 1"),
        "http://d.org" -> Some("Apache"),
        "http://e.org" -> None,
        "http://f.org" -> Some("nginx"),
        "http://g.org" -> Some("nginx")
      )

      def fetchUrls() = Future {
        Thread.sleep(300)
        serverNames.keys.toList.map(url => new URL(url))
      }(executionContext)

      def fetchServerName(url: URL) = Future {
        Thread.sleep(url.toString.length % 7 * 10)
        serverNames(url.toString)
      }(executionContext)

      fetchLinkedHttpServerCounts(fetchUrls, fetchServerName) map {
        counts => {
          val expected = expectedCounts(serverNames)
          info(s"expect server name counts: $expected")
          counts shouldEqual expected
        }
      }
    }

    it("should count large number server names") {
      // NOTE: Don't use a Map[URL, Option[String]] as using URLs as keys is very slow as URL.hashCode does a
      // DNS lookup.
      lazy val serverNames: Map[String, Option[String]] = {
        import util.Random

        val lookup = Map[Int, String](
          0 -> "Apache",
          1 -> "nginx",
          2 -> "IIS",
          3 -> "varnish"
        )

        val random = new Random(123) // Ensure we get the same sequence of numbers for every test run.
        Seq.fill(12345)(s"http://${random.nextInt.abs}.org")
          .map(url => url -> lookup.get(url.hashCode % 5))
      }.toMap

      def fetchUrls() = Future {
        // NOTE: To avoid slow performance due to URL.hashCode, first convert to List and only then create URLs
        serverNames.keys.toList.map(u => new URL(u))
      }(executionContext)

      def fetchServerName(url: URL) = Future {
        Thread.sleep(url.toString.length % 2)
        serverNames(url.toString)
      }(executionContext)

      fetchLinkedHttpServerCounts(fetchUrls, fetchServerName) map {
        counts => {
          val expected = expectedCounts(serverNames)
          info(s"expect server name counts: $expected")
          counts shouldEqual expected
        }
      }
    }
  }

  describe("Exercise 12") {
    import chapter.Ex12._

    it("should return simulated results") {
      def fetchLinks(url: URL): Future[List[String]] = Future {
        val random = new Random(url.toString.hashCode())
        (for (i <- 1 to 10) yield random.nextInt.abs)
          .map { r => s"http://$r.org" }
          .toList
      }(concurrentExecutionContext)

      def processLink(link: String): String = {
        s"Result for $link"
      }

      val eventualRes = simulateWithPromises(new URL("http://this-is-a-fake-url.org"), fetchLinks, processLink)
      Future.sequence(eventualRes).map {
        r => {
          r shouldEqual List("Result for http://1783571453.org", "Result for http://1320690939.org",
            "Result for http://2041168423.org", "Result for http://1688959843.org", "Result for http://726562605.org",
            "Result for http://9894546.org", "Result for http://84789648.org", "Result for http://1814152839.org",
            "Result for http://1357495115.org", "Result for http://48664380.org")
        }
      }
    }
  }

  describe("Exercise 13") {
    import chapter.Ex13._

    it("should find palindromic prime and immediately stop all workers") {
      val workerCount = 4
      val firstWorkerResults = immutable.Map(
        1 -> 1003001,
        2 -> 1250521,
        3 -> 1508051,
        4 -> 1755571
      )

      val eventualRes = findFirstPalindromicPrime(1 * 1000 * 1000, 2 * 1000 * 1000, workers = workerCount)
      eventualRes map {
        primeAndStats => {
          // Sleep for a while so as to ensure workers would have time to progress if they did not terminate once
          // one of them found a palindromic prime.
          Thread.sleep(2 * 1000)

          // This is not a 100% reliable test setup but it "works on my machine".
          primeAndStats._1 shouldEqual firstWorkerResults(2)

          val stats = primeAndStats._2.toMap
          stats.keySet.size shouldEqual workerCount

          // The same note about reliability applies here. It is impossible to say exact evaluation counts for workers
          // except for the one that finds the first palindromic prime (assuming of course that it is always the same
          // prime that is found first). However, it is enough to verify that evaluation counts are well below the
          // upper limit of each worker.
          stats(1) shouldBe <(3000)
          stats(2) shouldEqual 522
          stats(3) shouldBe <(3000)
          stats(4) shouldBe <(3000)
        }
      }
    }
  }
}
