package fi.kapsi.kosmik.sfti.ch11

/**
  * Implement a class BitSequence that stores a sequence of 64 bits packed in a Long
  * value. Supply apply and update operators to get and set an individual bit.
  */
object Ex07 {

  class BitSequence {
    private var packed: Long = 0

    def apply(bit: Int): Boolean = {
      validateBit(bit)
      val value = packBit(bit)
      (packed & value) == value
    }

    def update(bit: Int, value: Boolean): Unit = {
      validateBit(bit)
      if (value) packed = packed | packBit(bit)
      else packed = packed & ~packBit(bit)
    }

    private def packBit(bit: Int) = math.pow(2, bit).longValue

    private def validateBit(bit: Int): Unit =
      if (bit < 0 || bit > 63) throw new IllegalArgumentException(s"bit $bit is out of range")
  }

  object BitSequence {
    def apply(): BitSequence = new BitSequence()
  }

}
