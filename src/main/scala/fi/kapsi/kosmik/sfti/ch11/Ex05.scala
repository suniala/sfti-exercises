package fi.kapsi.kosmik.sfti.ch11

/**
  * Provide operators that construct an HTML table.
  */
object Ex05 {

  class Table(private val rows: List[List[String]] = List(List())) {
    def |(cellContent: String): Table =
      new Table(rows.updated(rows.length - 1, rows.last ++ List(cellContent)))

    def ||(cellContent: String): Table =
      new Table(rows ++ List(List(cellContent)))

    override def toString: String = {
      "<table>" +
        rows.map(cells => s"<tr>${cells.map(cell => s"<td>$cell</td>").mkString("")}</tr>").mkString("") +
        "</table>"
    }
  }

  object Table {
    def apply(): Table = new Table()
  }

}
