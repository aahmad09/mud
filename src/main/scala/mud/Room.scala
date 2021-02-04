package mud

import scala.io.Source

class Room(name: String, desc: String, private val exits: Array[Int], private var items: List[Item]) {
  def fullDescription(): String = ???

  def getExit(dir: Int): Option[Room] = ???

  def getItem(itemName: String): Option[Item] = {
    items.find(_.name == itemName) match {
      case Some(item) =>
        val index = items.indexOf(item)
        items = items.patch(index, Nil, 1)
        Some(item)
      case None => None
    }
  }

  def dropItem(item: Item): Unit = ???

}

object Room {
  val rooms: Array[Room] = readRooms()

  def readRooms(): Array[Room] = ???
}