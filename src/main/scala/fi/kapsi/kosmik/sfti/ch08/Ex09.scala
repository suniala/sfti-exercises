package fi.kapsi.kosmik.sfti.ch08

/**
  * In the Creature class of Section 8.10, “Construction Order and Early Definitions,”
  * on page 98, replace val range with a def . What happens when you also use a def
  * in the Ant subclass? What happens when you use a val in the subclass? Why?
  */
object Ex09 {

  class CreatureA {
    val range: Int = 10
    val env: Array[Int] = new Array[Int](range)
  }

  class AntA extends CreatureA {
    override val range = 2
  }

  class CreatureB {
    def range: Int = 10
    val env: Array[Int] = new Array[Int](range)
  }

  class AntB extends CreatureB {
    override val range = 2
  }

  class CreatureC {
    def range: Int = 10
    val env: Array[Int] = new Array[Int](range)
  }

  class AntC extends CreatureC {
    override def range = 2
  }

}
