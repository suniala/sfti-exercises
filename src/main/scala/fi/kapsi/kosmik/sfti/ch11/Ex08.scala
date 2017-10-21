package fi.kapsi.kosmik.sfti.ch11

/**
  * Provide a class Matrix . Choose whether you want to implement 2 × 2 matrices,
  * square matrices of any size, or m × n matrices. Supply operations + and * . The
  * latter should also work with scalars, for example, mat * 2 . A single element
  * should be accessible as mat(row, col) .
  */
object Ex08 {

  class Matrix(val m: Int, val n: Int) {
    private val matrix: Array[Array[Int]] = {
      val rows = new Array[Array[Int]](m)
      for (row <- 0 until m) rows(row) = new Array[Int](n)
      rows
    }

    /**
      * @param row row index from 1 to m
      * @param col col index from 1 to n
      * @return cell
      */
    def apply(row: Int, col: Int): Int = matrix(index(row))(index(col))

    /**
      * @param row   row index from 1 to m
      * @param col   col index from 1 to n
      * @param value cell value
      */
    def update(row: Int, col: Int, value: Int): Unit = matrix(index(row))(index(col)) = value

    def +(other: Matrix): Matrix = {
      if (m != other.m || n != other.n)
        throw new IllegalArgumentException(s"matrices $this and $other are incompatible for addition")

      val summed = Matrix(m, n)
      for (row <- 1 to m) {
        for (col <- 1 to n) {
          summed(row, col) = this (row, col) + other(row, col)
        }
      }

      summed
    }

    def *(other: Matrix): Matrix = {
      val a = this
      val b = other
      if (a.m != b.n || a.n != b.m)
        throw new IllegalArgumentException(s"matrices $a and $b are incompatible for multiplication")

      val multiplied = Matrix(a.m, b.n)

      def recMul(rowA: Int, colA: Int, rowB: Int, colB: Int): Unit = {
        multiplied(rowA, colB) += a(rowA, colA) * b(rowB, colB)

        if (rowB < b.m) {
          recMul(rowA, colA + 1, rowB + 1, colB)
        } else {
          if (colB < b.n) {
            recMul(rowA, 1, 1, colB + 1)
          } else if (rowA < a.m) {
            recMul(rowA + 1, 1, 1, 1)
          } else {
            ()
          }
        }
      }

      recMul(1, 1, 1, 1)
      multiplied
    }

    def *(multiplier: Int): Matrix = {
      val multiplied = Matrix(m, n)

      for (row <- 1 to m) {
        for (col <- 1 to n) {
          multiplied(row, col) = multiplier * this (row, col)
        }
      }

      multiplied
    }

    override def equals(obj: scala.Any): Boolean =
      obj match {
        case other: Matrix =>
          m == other.m &&
            n == other.n &&
            matrix
              .zip(other.matrix)
              .forall({ case (ra, rb) =>
                ra
                  .zip(rb)
                  .forall({ case (a, b) => a == b })
              })
        case _ => false
      }

    override def toString: String =
      "\n[\n" + matrix.map(row => row.map(cell => cell.toString).mkString(" ")).mkString("\n") + "\n]\n"

    private def index(coord: Int) = coord - 1
  }

  object Matrix {
    def apply(m: Int, n: Int): Matrix = new Matrix(m, n)

    def parse(formatted: String): Matrix = {
      def parseIntoFormattedRows(formattedMatrix: String) = formattedMatrix.split("\n").filterNot(_.trim.isEmpty)

      def parseIntoCells(formattedRow: String) = formattedRow.split("\\s").filterNot(_.trim.isEmpty).map(_.toInt)

      val formattedRows = parseIntoFormattedRows(formatted)
      val m = formattedRows.length
      val n = parseIntoCells(formattedRows.head).length

      val matrix = Matrix(m, n)
      for (row <- 1 to formattedRows.length) {
        val cells = parseIntoCells(formattedRows(row - 1))
        for (cell <- 1 to cells.length) {
          matrix(row, cell) = cells(cell - 1)
        }
      }

      matrix
    }
  }

}
