package fi.kapsi.kosmik.sfti

object Chapter11 {

  /**
    * Implement the Fraction class with operations + - * / . Normalize fractions, for
    * example, turning 15/–6 into –5/2. Divide by the greatest common divisor
    */
  object Ex03 {

    class Fraction(n: Int, d: Int) {
      private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)

      private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)

      override def toString = s"$num/$den"

      def sign(a: Int): Int = if (a > 0) 1 else if (a < 0) -1 else 0

      def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)

      final override def equals(obj: scala.Any): Boolean = {
        obj match {
          case other: Fraction => num == other.num && den == other.den
          case _ => false
        }
      }

      def +(other: Fraction): Fraction = Fraction(num * other.den + other.num * den, den * other.den)

      def -(other: Fraction): Fraction = Fraction(num * other.den - other.num * den, den * other.den)

      def unary_-(): Fraction = Fraction(-num, den)

      def *(other: Fraction): Fraction = Fraction(num * other.num, den * other.den)

      def /(other: Fraction): Fraction = {
        val otherCommonDen = other.num * other.den
        Fraction(num * otherCommonDen / other.num, den * otherCommonDen / other.den)
      }
    }

    object Fraction {
      def apply(n: Int, d: Int) = new Fraction(n, d)
    }

  }

  /**
    * Implement a class Money with fields for dollars and cents. Supply + , - operators
    * as well as comparison operators == and < . For example, Money(1, 75) + Money(0,
    * 50) == Money(2, 25) should be true . Should you also supply * and / operators?
    * Why or why not?
    */
  object Ex04 {

    class Money(val dollarSum: Int, val centSum: Int) {
      val dollars: Int = dollarSum + centSum / 100
      val cents: Int = centSum - centSum / 100 * 100

      def +(other: Money): Money = Money(0, 100 * (dollars + other.dollars) + cents + other.cents)

      def -(other: Money): Money = Money(0, 100 * (dollars - other.dollars) + cents - other.cents)

      final override def equals(other: Any): Boolean = {
        other match {
          case that: Money => dollars == that.dollars && cents == that.cents
          case _ => false
        }
      }

      def <(other: Money): Boolean =
        if (dollars < other.dollars) true
        else if (dollars == other.dollars) cents < other.cents
        else false

      override def toString: String = s"$$$dollars.$cents"

      /**
        * Provide an explicit multiplication method. A multiplier operator can not be used here as the other operand
        * is of another type (in this case Double) and operations like "2.0 * Money(2, 43)" would not work as this
        * operator associates to the left.
        *
        * @param multiplier multiply with this
        * @return multiplied money
        */
      def mul(multiplier: Double): Money =
        Money(0, math.round(multiplier * (100 * dollars + cents)).toInt)
    }

    object Money {
      def apply(dollarSum: Int, centSum: Int) = new Money(dollarSum, centSum)
    }

  }

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

  /**
    * Define an object PathComponents with an unapply operation class that extracts
    * the directory path and file name from an java.nio.file.Path . For example, the
    * file /home/cay/readme.txt has directory path /home/cay and file name readme.txt .
    */
  object Ex09 {

    object PathComponents {
      def unapply(path: java.nio.file.Path): Option[(String, String)] = {
        if (path.getNameCount == 0) None
        else Some((path.getParent.toString, path.getFileName.toString))
      }
    }

  }

  /**
    * Modify the PathComponents object of the preceding exercise to instead define an
    * unapplySeq operation that extracts all path segments. For example, for the file
    * /home/cay/readme.txt , you should produce a sequence of three segments: home ,
    * cay , and readme.txt .
    */
  object Ex10 {

    object SeqPathComponents {
      def unapplySeq(path: java.nio.file.Path): Option[Seq[String]] = {
        if (path.getNameCount == 0) None
        else {
          import scala.collection.JavaConverters._
          val es = for (e <- path.asScala) yield e.toString
          Some(es.toSeq)
        }
      }
    }

  }

  /**
    * Improve the dynamic property selector in Section 11.11, “Dynamic Invoca-
    * tion,” on page 150 so that one doesn’t have to use underscores. For example,
    * sysProps.java.home should select the property with key "java.home" . Use a helper
    * class, also extending Dynamic , that contains partially completed paths.
    */
  object Ex11 {

    import scala.language.dynamics

    class PartialPath(val props: java.util.Properties, path: String) extends Dynamic {
      def selectDynamic(name: String): PartialPath = {
        new PartialPath(props, s"$path.$name")
      }

      def updateDynamic(name: String)(value: String): Unit =
        props.setProperty(s"$path.$name", value)

      /**
        * Seems like there is no other option but to use an explicit toString to get a string value out of our dynamic
        * and recursive property selection dsl.
        *
        * @return property value
        */
      override def toString: String = props.getProperty(path)
    }

    class DynamicProps(val props: java.util.Properties) extends Dynamic {
      def selectDynamic(name: String): PartialPath =
        new PartialPath(props, name)
    }

  }

  /**
    * Define a class XMLElement that models an XML element with a name, attributes,
    * and child elements. Using dynamic selection and method calls, make it pos-
    * sible to select paths such as rootElement.html.body.ul(id="42").li , which should
    * return all li elements inside ul with id attribute 42 inside body inside html .
    */
  object Ex12 {

    import scala.collection.mutable
    import scala.language.dynamics

    class XMLElementSeq(private val elements: List[XMLElement]) extends Dynamic with Iterable[XMLElement] {
      override def iterator: Iterator[XMLElement] = elements.iterator

      def selectDynamic(name: String): XMLElementSeq =
        new XMLElementSeq(elements.flatMap(e => e.children.filter(c => c.label == name)))

      def applyDynamicNamed(name: String)(args: (String, String)*): XMLElementSeq = {
        val filtered = selectDynamic(name).elements.filter(e => {
          args.forall({ case (k, v) => e.attr(k) match {
            case Some(value) => value == v
            case _ => false
          }
          })
        })
        new XMLElementSeq(filtered)
      }
    }

    class XMLElement(val label: String, private val attributes: List[(String, String)]) extends Dynamic {
      val children: mutable.ListBuffer[XMLElement] = mutable.ListBuffer()

      def selectDynamic(name: String): XMLElementSeq =
        new XMLElementSeq(List(this)).selectDynamic(name)

      def attr(attrName: String): Option[String] = {
        val found = attributes.find({ case (name, _) => name == attrName })
        found match {
          case Some((_, value)) => Some(value)
          case _ => None
        }
      }
    }

    object XMLElement {
      def fromScalaXml(elem: scala.xml.Elem): XMLElement = {
        def rec(scalaNode: scala.xml.Node, parent: XMLElement): Unit = {
          val attributes = scalaNode.attributes.map(a => (a.key, a.value.toString)).toList
          val element = new XMLElement(scalaNode.label, attributes)

          parent.children.append(element)
          scalaNode.child.foreach(c => {
            rec(c, element)
          })
        }

        val root = new XMLElement("root", Nil)
        rec(elem, root)
        root
      }
    }

  }

  /**
    * Provide an XMLBuilder class for dynamically building XML elements, as
    * builder.ul(id="42", style="list-style: lower-alpha;") , where the method name
    * becomes the element name and the named arguments become the attributes.
    * Come up with a convenient way of building nested elements.
    */
  object Ex13 {

    import fi.kapsi.kosmik.sfti.Chapter11.Ex12.XMLElement

    import scala.language.dynamics

    class XMLBuilder extends Dynamic {
      /**
        * Build element.
        *
        * @param label element label
        * @return xml element
        */
      def selectDynamic(label: String): XMLElement =
        applyDynamicNamed(label)()

      /**
        * Build element and add attributes and child elements to it.
        *
        * @param label element label
        * @param args  named arguments are added as attributes, unnamed arguments of type XMLElement are added as
        *              children
        * @return xml element
        */
      def applyDynamicNamed(label: String)(args: (String, Any)*): XMLElement = {
        val (attrArgs, otherArgs) = args.partition({ case (key, _) => !key.isEmpty })
        val attrs = attrArgs.map({ case (key, value) => (key, value.toString) }).toList
        val children = otherArgs
          .map({ case (_, value) => value })
          .filter(value => value.isInstanceOf[XMLElement])
          .map(value => value.asInstanceOf[XMLElement])

        val element = new XMLElement(label, attrs)
        element.children.appendAll(children)
        element
      }
    }

  }

}