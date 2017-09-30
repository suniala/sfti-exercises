package fi.kapsi.kosmik.sfti.ch08

/**
  * Compile the Person and SecretAgent classes in Section 8.6, “Overriding Fields,”
  * on page 95 and analyze the class files with javap . How many name fields are
  * there? How many name getter methods are there? What do they get? (Hint:
  * Use the -c and -private options.)
  */
object Ex08 {

  /**
    * One field "name" with one getter "name()"
    */
  class Person(val name: String) {
    override def toString = s"${getClass.getName}[name=$name]"
  }

  /**
    * Two fields "name" and "toString" both of which have one getter each.
    */
  class SecretAgent(codename: String) extends Person(codename) {
    override val name = "secret"
    override val toString = "secret"
  }

}
