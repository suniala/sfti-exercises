package fi.kapsi.kosmik.sfti.ch11

import scala.language.dynamics

/**
  * Improve the dynamic property selector in Section 11.11, “Dynamic Invoca-
  * tion,” on page 150 so that one doesn’t have to use underscores. For example,
  * sysProps.java.home should select the property with key "java.home" . Use a helper
  * class, also extending Dynamic , that contains partially completed paths.
  */
object Ex11 {

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
