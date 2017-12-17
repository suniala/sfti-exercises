package fi.kapsi.kosmik.sfti.apps

import java.util.concurrent.Executors

import fi.kapsi.kosmik.sfti.Chapter17.Ex08.Quit
import fi.kapsi.kosmik.sfti.Chapter17.Ex09._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Failure, Success}

/**
  * App for chapter 17, exercise 9.
  */
object LinkedHttpServerStats extends App {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val url = if (args.length == 1) Some(args(0)) else None

  val eventualResult = fetchLinkedHttpServerCounts(url)
  Await.ready(eventualResult, Duration.Inf)

  eventualResult.value.get match {
    case Success(serverCounts) =>
      if (serverCounts.isEmpty) println("Could not find any servers.")
      else {
        println("Found servers: ")
        for ((s, c) <- serverCounts) println(s"  $s $c times")
      }
      System.exit(0)
    case Failure(ex: Quit) =>
      println("Bye!")
      System.exit(0)
    case Failure(ex) =>
      println("Extracting links failed because of: " + ex.toString)
      System.exit(1)
  }
}
