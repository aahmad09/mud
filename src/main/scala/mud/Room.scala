package mud

import scala.io.Source

class Room(val name: String, val desc: String, private val exits: Array[Int],  private var items: List[Item]) {

  def fullDescription(): String = name + "\n" +
                                    desc +"\n" +
                                    exits.mkString(", ") + "\n" +
                                    formatItem(items)


  def formatItem(unformattedItems:List[Item]):String = {
    var itemStr:String = ""
    for (elem <- unformattedItems) itemStr += elem.itemName + " \n" + elem.itemDesc + "\n"
    itemStr
  }

//  def getExit(dir: Int): Option[Room] = {
//    items.find(_.name == itemName) match {
//      case Some(item) =>
//        val index = items.indexOf(item)
//        items = items.patch(index, Nil, 1)
//        Some(item)
//      case None => None
//    }
//  }

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

  def readRooms(): Array[Room] = {
    val source = Source.fromFile("resources/map.txt")
    val lines = source.getLines()
    val ret = Array.fill(lines.next().toInt)(readRoom(lines))
    source.close()
    ret
  }

  def readRoom(lines: Iterator[String]): Room = {
    val name = lines.next()
    val desc = lines.next()
    val exits = lines.next().split(",").map(_.toInt)
    val items = List.fill(lines.next().toInt)(Item(lines.next(), lines.next()))
    new Room(name, desc, exits, items)
  }

}
