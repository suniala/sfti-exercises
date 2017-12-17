package fi.kapsi.kosmik.sfti.apps

import java.util.concurrent.Executors

import fi.kapsi.kosmik.sfti.Chapter17.Ex08.{Quit, extractLinks}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Failure, Success}

/**
  * App for chapter 17, exercise 8.
  */
object ExtractLinks extends App {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val url = if (args.length == 1) Some(args(0)) else None

  val eventualResult = extractLinks(url)
  Await.ready(eventualResult, Duration.Inf)

  eventualResult.value.get match {
    case Success(Nil) =>
      println(s"Could not find any links in document.")
      System.exit(0)
    case Success(c :: cs) =>
      println("Found links: ")
      for (link <- c :: cs) println(s"  $link")
      System.exit(0)
    case Failure(ex: Quit) =>
      println("Bye!")
      System.exit(0)
    case Failure(ex) =>
      println("Extracting links failed because of: " + ex.toString)
      System.exit(1)
  }
}
