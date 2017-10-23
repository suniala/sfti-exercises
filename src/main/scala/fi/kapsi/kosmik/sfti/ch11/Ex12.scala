package fi.kapsi.kosmik.sfti.ch11

import scala.collection.mutable
import scala.language.dynamics

/**
  * Define a class XMLElement that models an XML element with a name, attributes,
  * and child elements. Using dynamic selection and method calls, make it pos-
  * sible to select paths such as rootElement.html.body.ul(id="42").li , which should
  * return all li elements inside ul with id attribute 42 inside body inside html .
  */
object Ex12 {

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
