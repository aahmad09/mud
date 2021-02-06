package mud

import scala.io.Source

class Room(val name: String, val desc: String, private val exits: Array[Int], private var items: List[Item]) {

  //Print the complete description of the room.
  def fullDescription(): Unit = println(name + "\n" +
    desc + "\n" +
    "Exits: " + formatExits(exits) +
    "Items: " + formatItem(items))

  //Format item names and desc for printing
  def formatItem(unformattedItems: List[Item]): String = {
    var itemStr: String = ""
    for (elem <- unformattedItems) itemStr += elem.itemName + ", "
//    + " \n" + elem.itemDesc + "\n"
    if (itemStr == "") itemStr = "None  "
    itemStr.dropRight(2)
  }

  //Transform exit numbers into direction names for printing
  def formatExits(unformattedExits: Array[Int]): String = {
    //north=0, south=1, east=2, west=3, up=4, and down=5
    var exitStr: String = ""

    unformattedExits.foreach {
      case 0 => exitStr += "North, "
      case 1 => exitStr += "South, "
      case 2 => exitStr += "East, "
      case 3 => exitStr += "West, "
      case 4 => exitStr += "Up, "
      case 5 => exitStr += "Down, "
      case _ =>
    }

    exitStr.dropRight(2) + "\n"
  }

  //Return the room in a given direction if it exists
  def getExit(dir: Int): Option[Room] = {
    if (exits(dir) == -1) None
    else Some(Room.rooms(exits(dir)))
  }

  // Get item from room if it exists and remove it from the room
  def getItem(itemName: String): Option[Item] = {
    items.find(_.name.toLowerCase == itemName) match {
      case Some(item) =>
        val index = items.indexOf(item)
        items = items.patch(index, Nil, 1)
        Some(item)
      case None => None
    }
  }

  //Add an item to this room
  def dropItem(item: Item): Unit = item :: items

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
