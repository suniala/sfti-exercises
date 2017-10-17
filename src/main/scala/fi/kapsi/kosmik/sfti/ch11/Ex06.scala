package fi.kapsi.kosmik.sfti.ch11

/**
  * Provide a class ASCIIArt... Supply operators for combining two ASCIIArt figures horizontally or vertically.
  * Choose operators with appropriate precedence.
  */
object Ex06 {

  class ASCIIArt(private val rows: List[String] = List()) {
    def :\(other: ASCIIArt): ASCIIArt =
      new ASCIIArt(rows ++ other.rows)

    def :-(other: ASCIIArt): ASCIIArt = {
      val leftWidth = rows.map(_.length).max
      new ASCIIArt(
        rows
          .zipAll(other.rows, "", "")
          .map({ case (left, right) => left + " " * (leftWidth - left.length + 1) + right })
      )
    }

    override def toString: String = rows.mkString("\n")
  }

  private def parseTemplate(template: String): List[String] =
    template.split("\n").toList

  private def loadTemplate(name: String): List[String] =
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(name + ".txt")).getLines().toList

  object Cat extends ASCIIArt {
    def apply(): ASCIIArt = new ASCIIArt(loadTemplate("cat"))
  }

  object Hello extends ASCIIArt {
    def apply(): ASCIIArt = new ASCIIArt(loadTemplate("hello"))
  }

}
