package fi.kapsi.kosmik.sfti.test

import java.time.{Clock, Duration, Instant}

object StopWatch {
  def start: StopWatch = new StopWatch()
}

class StopWatch {
  private val origin = Clock.systemUTC().instant()

  def <(duration: Duration): Boolean = {
    Clock.systemUTC().instant().minus(duration).isBefore(origin)
  }

  def split(): Split = {
    new Split(origin, Clock.systemUTC().instant())
  }

  override def toString: String = "StopWatch started at " + origin
}

class Split(start: Instant, end: Instant) {
  def <(duration: Duration): Boolean = {
    end.minus(duration).isBefore(start)
  }

  override def toString: String = Duration.ofMillis(end.minusMillis(start.toEpochMilli).toEpochMilli).toString
}
