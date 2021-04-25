package mud

import akka.actor.{Actor, ActorRef, Props}
import mud.NPCManager.CreateNPC

class RoomManager extends Actor {

  import RoomManager._

  val roomsMap: Map[String, ActorRef] = readRooms()
  context.children.foreach(child => child ! Room.LinkExits(roomsMap))

  def receive: Receive = {
    case BeginGame =>
      sender ! Player.StartRoom(roomsMap("void_"))
    case m => println("Unhandled message in RoomManager " + m)
  }

  def readRooms(): Map[String, ActorRef] = {
    val xmlData = xml.XML.loadFile("resources/map.xml")
    (xmlData \ "room").map(readRoom).toMap
  }

  def readRoom(node: xml.Node): (String, ActorRef) = {
    val key = (node \ "@keyword").text
    val name = (node \ "@name").text
    val desc = (node \ "description").text
    val exits = (node \ "exits").text.split(",")
    val items = (node \ "item").map(n => Item((n \ "@name").text, (n \ "@desc").text,
      (n \ "@dmg").text.toInt, (n \ "@delay").text.toInt)).toList
    val ret = key -> context.actorOf(Props(new Room(name, desc, exits, items)), key)
    (node \ "npc").foreach(x => Main.npcManager ! CreateNPC((x \ "@name").text, x.text, ret._2))
    ret
  }

}

object RoomManager {

  case object BeginGame

}