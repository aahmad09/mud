package mud

import akka.actor.{Actor, ActorRef}

import scala.collection.mutable.ArrayBuffer

class Room(val name: String,
           val desc: String,
           private val exitKeys: Array[String],
           private var items: List[Item])
  extends Actor {

  private val users: ArrayBuffer[ActorRef] = ArrayBuffer()
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
    case AddPlayer(user: ActorRef) =>
      users += user
    case RemovePlayer(user: ActorRef) =>
      println(user)
      println(users.mkString(", "))
      users -= user
      println(users.mkString(", "))
    case BroadcastInRoom(playerName: String, msg: String) =>
      users.foreach(_ ! Player.PrintMessage(s"*** $playerName $msg ***"))
    case m => println("Unhandled message in Room " + m)
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

  //Print the complete description of the room.
  def fullDescription(): String = s"$name\n$desc\nExits: ${formatExits()}Items: ${formatItem(items)} " +
    s"\nPlayers in this room: ${formatPlayers(users)}"

  def formatPlayers(unformattedList: ArrayBuffer[ActorRef]): String = {
    var ret = ""
    for (playerName <- unformattedList) ret += playerName.path.name + ", "
    if (ret == "") ret = "None  "
    ret.dropRight(2)
  }

  //Format item names and desc for printing
  def formatItem(unformattedItems: List[Item]): String = {
    var ret: String = ""
    for (elem <- unformattedItems) ret += elem.itemName + ", "
    if (ret == "") ret = "None  "
    ret.dropRight(2)
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

  //Add an item to this room
  def dropItem(item: Item): Unit = items = item :: items

}

object Room {

  case class LinkExits(rooms: Map[String, ActorRef])

  case class GetExit(dir: Int)

  case class GetItem(itemName: String)

  case class DropItem(item: Item)

  case class AddPlayer(user: ActorRef)

  case class RemovePlayer(user: ActorRef)

  case class PlayersInRoom(user: ActorRef)

  case class BroadcastInRoom(playerName: String, msg: String)

  case object FullDescription

}

