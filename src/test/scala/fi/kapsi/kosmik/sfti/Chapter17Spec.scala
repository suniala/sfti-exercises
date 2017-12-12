package fi.kapsi.kosmik.sfti

import java.time.Duration
import java.util.concurrent.Executors

import fi.kapsi.kosmik.sfti.test.StopWatch
import fi.kapsi.kosmik.sfti.{Chapter17 => chapter}
import org.scalatest.{AsyncFunSpec, Matchers}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

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
      futureSum map { sum => assert(sum == 42) }
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

      doInOrder(f, g)(3.2) map { v => assert(v == "###") }
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
      eventualInt map { v => assert(v == 4) }
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
          assert(aRes == 11111)
          assert(bRes == "xxxxx")
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
          assert(xs == results)
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
          assert(remainingInputs.length == 1)
          assert(pastInputs.length == 4)
          assert(validInput == "secret")
        }
      }
    }
  }

  describe("Exercise 07") {
    import chapter.Ex07._

    val performanceTestN = 5000000
    val performanceTestExpectedPrimes = 348513

    it("should count expected primes sequentially") {
      primesSequential(70) map {
        count => assert(count == 19)
      }
    }

    it("should count expected number of primes sequentially") {
      val stopWatch = StopWatch.start

      primesSequential(performanceTestN) map {
        count => {
          val split = stopWatch.split()
          info(f"counting $count primes sequentially took $split")
          assert(count == performanceTestExpectedPrimes)
        }
      }
    }

    it("should count expected primes concurrently") {
      primesConcurrentC(70) map {
        size => assert(size == 19)
      }
    }

    it("should count expected number of primes concurrently a") {
      val stopWatch = StopWatch.start

      primesConcurrentA(performanceTestN) map {
        count => {
          val split = stopWatch.split()
          info(f"counting $count primes concurrently a took $split")
          assert(count == performanceTestExpectedPrimes)
        }
      }
    }

    it("should count expected number of primes concurrently b") {
      val stopWatch = StopWatch.start

      primesConcurrentB(performanceTestN) map {
        count => {
          val split = stopWatch.split()
          info(f"counting $count primes concurrently b took $split")
          assert(count == performanceTestExpectedPrimes)
        }
      }
    }

    it("should count expected number of primes concurrently c") {
      val stopWatch = StopWatch.start

      primesConcurrentC(performanceTestN) map {
        count => {
          val split = stopWatch.split()
          info(f"counting $count primes concurrently c took $split")
          assert(count == performanceTestExpectedPrimes)
        }
      }
    }

    it("should count expected number of primes concurrently d") {
      val stopWatch = StopWatch.start

      primesConcurrentD(performanceTestN) map {
        count => {
          val split = stopWatch.split()
          info(f"counting $count primes concurrently d took $split")
          assert(count == performanceTestExpectedPrimes)
        }
      }
    }
  }
}
