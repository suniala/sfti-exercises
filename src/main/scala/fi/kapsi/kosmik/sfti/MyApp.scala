package fi.kapsi.kosmik.sfti

import scala.concurrent.Await
import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global

object MyApp extends App {
  println("starting")
  val eventualResult = Chapter17.Ex07.primesCountConcurrent(5000000 * 2)
  val result = Await.result(eventualResult, 30.seconds)
  println(result)
  println("done")
}
