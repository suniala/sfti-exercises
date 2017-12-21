package fi.kapsi.kosmik.sfti.apps

import fi.kapsi.kosmik.sfti.Chapter17.Ex11._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * App for chapter 17, exercise 11.
  */
object DelayedPrintTimeDemo extends App {
  println("a) Number of threads equals available processors, with default execution context")
  Await.ready(delayedPrintTimeUsingDefaultEC(Runtime.getRuntime.availableProcessors()), Duration.Inf)

  println("b) 40 threads with default execution context")
  Await.ready(delayedPrintTimeUsingDefaultEC(40), Duration.Inf)

  println("c) 40 threads with thread pool execution context")
  Await.ready(delayedPrintTimeUsingThreadPoolEC(40), Duration.Inf)

  println("Done.")
}
