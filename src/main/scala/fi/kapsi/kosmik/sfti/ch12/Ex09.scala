package fi.kapsi.kosmik.sfti.ch12

/**
  * Implement corresponds without currying. Then try the call from the preceding
  * exercise. What problem do you encounter?
  **/
object Ex09 {

  class Corresponder(val xs: Array[String]) {
    def corresponds[B](that: Array[B], p: (String, B) => Boolean): Boolean =
      xs.corresponds(that)(p)
  }

}
