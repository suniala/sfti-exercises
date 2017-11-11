package fi.kapsi.kosmik.sfti

object Chapter08 {

  /**
    * Extend the following BankAccount class to a CheckingAccount class that charges $1
    * for every deposit and withdrawal.
    */
  object Ex01 {

    class BankAccount(initialBalance: Double) {
      private var balance = initialBalance

      def currentBalance: Double = balance

      def deposit(amount: Double): Double = {
        balance += amount
        balance
      }

      def withdraw(amount: Double): Double = {
        balance -= amount
        balance
      }
    }

    class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
      override def deposit(amount: Double): Double = super.deposit(amount - 1)

      override def withdraw(amount: Double): Double = super.withdraw(amount + 1)
    }

  }

  /**
    * Extend the BankAccount class of the preceding exercise into a class SavingsAccount
    * that earns interest every month (when a method earnMonthlyInterest is called)
    * and has three free deposits or withdrawals every month. Reset the transaction
    * count in the earnMonthlyInterest method.
    */
  object Ex02 {

    class SavingsAccount(initialBalance: Double) extends Ex01.BankAccount(initialBalance) {
      val freeTransactions: Int = 3

      var remainingFreeTransactions: Int = freeTransactions

      def earnMonthlyInterest(rate: Double): Double = {
        remainingFreeTransactions = freeTransactions
        super.deposit(rate * super.currentBalance)
      }

      private def transactionFee() = {
        val fee = if (remainingFreeTransactions > 0) 0.0 else 1.0
        remainingFreeTransactions -= 1
        fee
      }

      override def deposit(amount: Double): Double = super.deposit(amount - transactionFee())

      override def withdraw(amount: Double): Double = super.withdraw(amount + transactionFee())
    }

  }

  /**
    * Define an abstract class Item with methods price and description . A SimpleItem is
    * an item whose price and description are specified in the constructor. Take
    * advantage of the fact that a val can override a def . A Bundle is an item that
    * contains other items. Its price is the sum of the prices in the bundle. Also
    * provide a mechanism for adding items to the bundle and a suitable description
    * method.
    */
  object Ex04 {

    import scala.collection.mutable

    abstract class Item {
      def price: Double

      def description: String
    }

    class SimpleItem(val price: Double, val description: String) extends Item

    class Bundle extends Item {
      private val items: mutable.ListBuffer[Item] = mutable.ListBuffer[Item]()

      override def price: Double = items.map(_.price).sum

      override def description: String = items.map(_.description).mkString(", ")

      def add(item: Item): Bundle = {
        items += item
        this
      }
    }

  }

  /**
    * Design a class Point whose x and y coordinate values can be provided in a
    * constructor. Provide a subclass LabeledPoint whose constructor takes a label
    * value and x and y coordinates, such as
    * new LabeledPoint("Black Thursday", 1929, 230.07)
    */
  object Ex05 {

    class Point(val x: Double, val y: Double)

    class LabeledPoint(val label: String, override val x: Double, override val y: Double) extends Point(x, y)

  }

  /**
    * Define an abstract class Shape with an abstract method centerPoint and subclasses
    * Rectangle and Circle . Provide appropriate constructors for the subclasses and
    * override the centerPoint method in each subclass.
    */
  object Ex06 {

    type Coord = (Double, Double)

    abstract class Shape {
      def centerPoint: Coord
    }

    class Rectangle(val bottomLeft: Coord, val topRight: Coord) extends Shape {
      override def centerPoint: Coord = {
        (bottomLeft._1 + (topRight._1 - bottomLeft._1) / 2, bottomLeft._2 + (topRight._2 - bottomLeft._2) / 2)
      }
    }

    class Circle(val centerPoint: Coord, val radius: Double) extends Shape

  }

  /**
    * Provide a class Square that extends java.awt.Rectangle and has three constructors:
    * one that constructs a square with a given corner point and width, one
    * that constructs a square with corner (0, 0) and a given width, and one that
    * constructs a square with corner (0, 0) and width 0 .
    */
  object Ex07 {

    class Square(x: Int, y: Int, width: Int)
      extends java.awt.Rectangle(x, y, width, width) {

      def this(width: Int) {
        this(0, 0, width)
      }

      def this() {
        this(0)
      }
    }

  }

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

  /**
    * In the Creature class of Section 8.10, “Construction Order and Early Definitions,”
    * on page 98, replace val range with a def . What happens when you also use a def
    * in the Ant subclass? What happens when you use a val in the subclass? Why?
    */
  object Ex09 {

    class CreatureA {
      val range: Int = 10
      val env: Array[Int] = new Array[Int](range)
    }

    class AntA extends CreatureA {
      override val range = 2
    }

    class CreatureB {
      def range: Int = 10

      val env: Array[Int] = new Array[Int](range)
    }

    class AntB extends CreatureB {
      override val range = 2
    }

    class CreatureC {
      def range: Int = 10

      val env: Array[Int] = new Array[Int](range)
    }

    class AntC extends CreatureC {
      override def range = 2
    }

  }

  /**
    * The file scala/collection/immutable/Stack.scala contains the definition
    * class Stack[A] protected (protected val elems: List[A])
    * Explain the meanings of the protected keywords.
    */
  object Ex10 {

    import scala.collection.immutable.Stack

    class IntStack(initial: List[Int]) extends Stack[Int](initial) {
      def peek(): Int = {
        // Subclasses have access to the protected elems field.
        elems.head
      }
    }

    val intStack = new IntStack(List(1, 2, 3))

    // Protected elems field can only be accessed from subclasses. The following would not compile:
    // val initialInt = intStack.elems.head

    // Stack constructor can only be invoked by subclasses. The following would not compile:
    // val stack = new Stack[Int](List(1, 2, 3))
  }

  /**
    * Define a value class Point that packs integer x and y coordinates into a Long
    * (which you should make private).
    */
  object Ex11 {

    // To be honest, I could not figure out what is the point (pun intented) of this exercise.
  }

}