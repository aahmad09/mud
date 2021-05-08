package mud

import akka.actor.{Actor, ActorRef}

import scala.collection.mutable

class Room(val roomName: String,
           val desc: String,
           private val exitKeys: Array[String],
           private var items: List[Item])
  extends Actor {

  private var charactersMap: mutable.Map[String, ActorRef] = mutable.Map()
  private var exits: Array[Option[ActorRef]] = null

  import Room._

  def receive: Receive = {
    case LinkExits(rooms) =>
      sender ! RoomManager.ExitMap(self.path.name, exitKeys)
      exits = exitKeys.map(key => rooms.get(key))
    case FullDescription =>
      sender ! Player.PrintMessage(fullDescription())
    case GetExit(dir) =>
      sender ! Player.TakeExit(getExit(dir))
    case GetItem(itemName) =>
      sender ! Player.PickItem(getItem(itemName))
    case DropItem(item) =>
      dropItem(item)
    case AddCharacter(charName, user) =>
      charactersMap = charactersMap + (charName.toLowerCase -> user)
    case RemoveCharacter(charName, _) => //TODO: remove player from room when they disconnect
      charactersMap = charactersMap - charName.toLowerCase
    case FindCharacter(charName, weapon) =>
      sender ! Player.InitiateAttack(charactersMap.get(charName.toLowerCase), weapon)
    case GetCharStats(entityName, requester) =>
      charactersMap.get(entityName) match {
        case Some(found) => found ! Player.ReturnStats(requester)
      }
    case BroadcastInRoom(playerName, msg) =>
      charactersMap.foreach(_._2 ! Player.PrintMessage(s"$playerName $msg"))
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
  def fullDescription(): String = "*" * 40 + s"\n$roomName\n$desc\nExits: ${formatExits()}Items: ${formatItem(items)} " +
    s"\nCharacters in this room: $formatCharacters\n" + "*" * 40

  def formatCharacters: String = charactersMap.keys.mkString(", ")

  //Format item names and desc for printing
  def formatItem(unformattedItems: List[Item]): String = {
    var ret: String = ""
    unformattedItems.foreach(ret += _.name + ", ")
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

  case class LinkExits(rooms: BSTMap[String, ActorRef])

  case class GetExit(dir: Int)

  case class GetItem(itemName: String)

  case class DropItem(item: Item)

  case class AddCharacter(charName: String, user: ActorRef)

  case class RemoveCharacter(charName: String, user: ActorRef)

  case class FindCharacter(charName: String, weapon: Item)

  case class GetCharStats(entityName: String, sender: ActorRef)

  case class BroadcastInRoom(playerName: String, msg: String)

  case object FullDescription

  case object GetName

}

