package mud

import akka.actor.{Actor, ActorRef}

class Room(val name: String,
           val desc: String,
           private val exitKeys: Array[String],
           private var items: List[Item])
  extends Actor {

  private var exits: Array[Option[ActorRef]] = null

  import Room._

  def receive: Receive = {
    case LinkExits(rooms) =>
      exits = exitKeys.map(key => rooms.get(key))
    case FullDescription =>
      sender ! Player.PrintMessage(fullDescription())
    case GetExit(dir) =>
      sender ! Player.TakeExit(getExit(dir))
    case GetItem(itemName) =>
      sender ! Player.PickItem(getItem(itemName))
    case DropItem(item) =>
      dropItem(item)
    case m => println("Unhandled message in Room " + m)
  }

  //Print the complete description of the room.
  def fullDescription(): String = s"$name\n${wrap(desc)}\nExits: " + s"${formatExits()}Items: ${formatItem(items)}"

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
  def formatExits(): String = {
    //north=0, south=1, east=2, west=3, up=4, and down=5
    var exitStr: String = ""

    if (exits(0).isDefined) exitStr += "north, "
    if (exits(1).isDefined) exitStr += "south, "
    if (exits(2).isDefined) exitStr += "east, "
    if (exits(3).isDefined) exitStr += "west, "
    if (exits(4).isDefined) exitStr += "up, "
    if (exits(5).isDefined) exitStr += "down, "

    exitStr.dropRight(2) + "\n"
  }

  //Return the actor ref of room in a given direction if it exists
  def getExit(dir: Int): Option[ActorRef] = exits(dir)

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

  case class LinkExits(rooms: Map[String, ActorRef])

  case class GetExit(dir: Int)

  case class GetItem(itemName: String)

  case class DropItem(item: Item)

  case object FullDescription

}
