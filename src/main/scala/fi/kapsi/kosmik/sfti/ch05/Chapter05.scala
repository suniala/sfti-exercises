package fi.kapsi.kosmik.sfti.ch05

import scala.beans.BeanProperty

object Chapter05 {

  /**
    * Improve the Counter class in Section 5.1, “Simple Classes and Parameterless
    * Methods,” on page 55 so that it doesn’t turn negative at Int.MaxValue.
    */
  class Ex01Counter {
    private var value: Long = 0

    def increment() {
      value += 1
    }

    def current: Long = value
  }

  /**
    * Write a class BankAccount with methods deposit and withdraw, and a read-only
    * property balance.
    */
  class Ex02BankAccount {
    private var value: Double = 0.0

    def deposit(amount: Double): Unit = {
      value += amount
    }

    def balance: Double = value
  }

  /**
    * Write a class Time with read-only properties hours and minutes and a method
    * before(other: Time): Boolean that checks whether this time comes before the
    * other. A Time object should be constructed as new Time(hrs, min), where hrs is in
    * military time format (between 0 and 23).
    */
  class Ex03Time(val hours: Int, val minutes: Int) {
    def before(other: Ex03Time): Boolean = {
      if (hours == other.hours) minutes <= other.minutes
      else hours <= other.hours
    }
  }

  /**
    * Reimplement the Time class from the preceding exercise so that the internal
    * representation is the number of minutes since midnight (between 0 and 24 ×
    * 60 – 1). Do not change the public interface. That is, client code should be
    * unaffected by your change.
    */
  class Ex04Time {
    private var time = 0

    def this(hours: Int, minutes: Int) {
      this()
      this.time = hours * 60 + minutes
    }

    private def hours: Int = time / 60

    private def minutes: Int = time - hours * 60

    def before(other: Ex04Time): Boolean = {
      if (hours == other.hours) minutes <= other.minutes
      else hours <= other.hours
    }

    override def toString: String = f"$hours:$minutes"
  }

  /**
    * <p>
    * Make a class Student with read-write JavaBeans properties name (of type String )
    * and id (of type Long). What methods are generated? (Use javap to check.) Can
    * you call the JavaBeans getters and setters in Scala? Should you?
    * <p>
    * Answer: The generated Java class as reported by javap:
    * <pre>
    * public class fi.kapsi.kosmik.sfti.ch05.Chapter05$Ex05Student {
    * public long id();
    * public void id_$eq(long);
    * public java.lang.String name();
    * public void name_$eq(java.lang.String);
    * public long getId();
    * public void setId(long);
    * public java.lang.String getName();
    * public void setName(java.lang.String);
    * public fi.kapsi.kosmik.sfti.ch05.Chapter05$Ex05Student(long, java.lang.String);
    * }
    * </pre>
    */
  class Ex05Student(@BeanProperty var id: Long, @BeanProperty var name: String)

  /**
    * In the Person class of Section 5.1, “Simple Classes and Parameterless Methods,”
    * on page 55, provide a primary constructor that turns negative ages to 0.
    */
  class Ex06Person(private val initialAge: Int) {
    var age: Int = if (initialAge >= 0) initialAge else 0
  }

  /**
    * Write a class Person with a primary constructor that accepts a string containing
    * a first name, a space, and a last name, such as new Person("Fred Smith"). Supply
    * read-only properties firstName and lastName. Should the primary constructor
    * parameter be a var, a val, or a plain parameter? Why?
    * <p>
    * Answer: Use a private val as we don't want the parameter to be visible outside the class nor
    * modifiable.
    */
  class Ex07Person(private val formattedName: String) {
    def firstName: String = formattedName.split(" ").head

    def lastName: String = formattedName.split(" ").last
  }

  /**
    * Make a class Car with read-only properties for manufacturer, model name,
    * and model year, and a read-write property for the license plate. Supply four
    * constructors. All require the manufacturer and model name. Optionally,
    * model year and license plate can also be specified in the constructor. If not,
    * the model year is set to -1 and the license plate to the empty string. Which
    * constructor are you choosing as the primary constructor? Why?
    */
  class Ex08Car(val manufacturer: String, val model: String, val year: Int = -1, var license: String = "") {
    def this(manufacturer: String, model: String, license: String) {
      // NOTE: year parameter must be included here so as to avoid ambiguity between the two constructors
      this(manufacturer, model, -1, license)
    }
  }

}
