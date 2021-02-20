package mud

import scala.io.Source

class Room(val name: String,
           val desc: String,
           private val exits: Array[String],
           private var items: List[Item]) {

  //Print the complete description of the room.
  def fullDescription(): Unit = println(s"$name\n${wrap(desc)}\nExits: ${formatExits(exits)}Items: ${formatItem(items)}")

  //Format item names and desc for printing
  def formatItem(unformattedItems: List[Item]): String = {
    var itemStr: String = ""
    for (elem <- unformattedItems) itemStr += elem.itemName + ", "
    if (itemStr == "") itemStr = "None  "
    itemStr.dropRight(2)
  }

  def wrap(input: String, maxLength: Int = 100): String = {
    input.split(" ").foldLeft(("", 0))(
      (acc, in) =>
        if (in equals "") acc else if ((acc._2 + in.length()) < maxLength) {
          (acc._1 + " " + in, acc._2 + in.length())
        }
        else {
          (acc._1 + '\n' + in, in.length())
        })._1.trim
  }

  //Transform exit numbers into direction names for printing
  def formatExits(unformattedExits: Array[String]): String = {
    //north=0, south=1, east=2, west=3, up=4, and down=5
    var exitStr: String = ""

    if (unformattedExits(0) != "none") exitStr += "north, "
    if (unformattedExits(1) != "none") exitStr += "south, "
    if (unformattedExits(2) != "none") exitStr += "east, "
    if (unformattedExits(3) != "none") exitStr += "west, "
    if (unformattedExits(4) != "none") exitStr += "up, "
    if (unformattedExits(5) != "none") exitStr += "down, "

    exitStr.dropRight(2) + "\n"
  }

  //Return the room in a given direction if it exists
  def getExit(dir: Int): Option[Room] = {
    if (exits(dir) == "none") None
    else Some(Room.rooms(exits(dir)))
  }

  // Get item from room if it exists and remove it from the room
  def getItem(itemName: String): Option[Item] = {
    items.find(_.name.toLowerCase == itemName) match {
      case Some(item) =>
        items = items.patch(items.indexOf(item), Nil, 1)
        Some(item)
      case None => None
    }
  }

  //Add an item to this room
  def dropItem(item: Item): Unit = items = item :: items

}

object Room {
  val rooms: Map[String, Room] = readRooms()

  def readRooms(): Map[String, Room] = {
    val source = Source.fromFile("resources/map.txt")
    val lines = source.getLines()
    val ret = Array.fill(lines.next().toInt)(readRoom(lines)).toMap
    source.close()
    ret
  }

  def readRoom(lines: Iterator[String]): (String, Room) = {
    val keyword = lines.next()
    val name = lines.next()
    val desc = lines.next()
    val exits = lines.next.split(",").map(_.trim)
    val items = List.fill(lines.next().toInt)(Item(lines.next(), lines.next()))
    (keyword, new Room(name, desc, exits, items))
  }

}
