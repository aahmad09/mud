package mud

import akka.actor.{Actor, ActorRef, Props}

import scala.collection.mutable

class RoomManager extends Actor {

  import RoomManager._

  val roomsMap: BSTMap[String, ActorRef] = readRooms()
  val exitInfo: mutable.Map[String, Array[String]] = mutable.Map[String, Array[String]]()

  context.children.foreach(child => child ! Room.LinkExits(roomsMap))

  def receive: Receive = {
    case BeginGame =>
      sender ! Player.StartRoom(roomsMap("void_"))
    case ExitMap(key, exits) =>
      exitInfo += key -> exits
    case ShortestPath(currLoc, destLoc) =>
      sender ! Player.PrintMessage(shortestPath(currLoc, destLoc))
    case GetRooms =>
      sender ! Player.PrintMessage(exitInfo.keys.mkString(", "))
    case m => println("Unhandled message in RoomManager " + m)
  }

  def shortestPath(origLoc: String, destLoc: String): String = {
    var exitPath: List[String] = Nil

    def helper(currLoc: String, destLoc: String, visited: Set[String]): List[String] = {
      if (currLoc == destLoc) return Nil
      else {
        for (i <- exitInfo(currLoc); if i != "none" && !visited(i)) yield {
          exitInfo(currLoc).indexOf(i) match {
            case 0 => exitPath = "north" :: helper(i, destLoc, visited + currLoc)
            case 1 => exitPath = "south" :: helper(i, destLoc, visited + currLoc)
            case 2 => exitPath = "east" :: helper(i, destLoc, visited + currLoc)
            case 3 => exitPath = "west" :: helper(i, destLoc, visited + currLoc)
            case 4 => exitPath = "up" :: helper(i, destLoc, visited + currLoc)
            case 5 => exitPath = "down" :: helper(i, destLoc, visited + currLoc)
            case _ =>
          }
        }
      }
      exitPath
    }

    //build return strings
    if (exitInfo.contains(origLoc) && exitInfo.contains(destLoc)) {
      s"Shortest Path from $origLoc to $destLoc\n" + helper(origLoc, destLoc, Set.empty[String]).mkString(", ")
    } else "Could not find these regions"
  }

  def readRooms(): BSTMap[String, ActorRef] = {
    val xmlData = xml.XML.loadFile("resources/map.xml")
    val retMap = new BSTMap[String, ActorRef](_ < _)
    val data = (xmlData \ "room").map(readRoom)
    for ((name, actor) <- data) retMap += name -> actor
    retMap
  }

  def readRoom(node: xml.Node): (String, ActorRef) = {
    val key = (node \ "@keyword").text
    val name = (node \ "@name").text
    val desc = (node \ "description").text
    val exits = (node \ "exits").text.split(",")
    val items = (node \ "item").map(n => Item((n \ "@name").text, (n \ "@desc").text,
      (n \ "@dmg").text.toInt, (n \ "@delay").text.toInt)).toList
    val ret = key -> context.actorOf(Props(new Room(name, desc, exits, items)), key)
    (node \ "npc").foreach(n => Main.npcManager ! NPCManager.CreateNPC(name = (n \ "@name").text, desc = (n \ "@desc").text, weapon = Item((n \ "@itemName").text, (n \ "@itemDesc").text,
      (n \ "@itemDmg").text.toInt, (n \ "@itemDelay").text.toInt), location = ret._2))
    ret
  }

}

object RoomManager {

  case class ShortestPath(currLoc: String, destLoc: String)

  case class ExitMap(key: String, exits: Array[String])

  case object BeginGame

  case object GetRooms

}