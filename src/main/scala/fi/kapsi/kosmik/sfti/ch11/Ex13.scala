package fi.kapsi.kosmik.sfti.ch11

import fi.kapsi.kosmik.sfti.ch11.Ex12.XMLElement

import scala.language.dynamics

/**
  * Provide an XMLBuilder class for dynamically building XML elements, as
  * builder.ul(id="42", style="list-style: lower-alpha;") , where the method name
  * becomes the element name and the named arguments become the attributes.
  * Come up with a convenient way of building nested elements.
  */
object Ex13 {

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
