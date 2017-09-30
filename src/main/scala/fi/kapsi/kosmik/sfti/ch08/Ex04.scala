package fi.kapsi.kosmik.sfti.ch08

import scala.collection.mutable

/**
  * Define an abstract class Item with methods price and description . A SimpleItem is
  * an item whose price and description are specified in the constructor. Take
  * advantage of the fact that a val can override a def . A Bundle is an item that
  * contains other items. Its price is the sum of the prices in the bundle. Also
  * provide a mechanism for adding items to the bundle and a suitable description
  * method.
  */
object Ex04 {

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
