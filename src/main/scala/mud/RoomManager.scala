package mud

import akka.actor.{Actor, ActorRef, Props}

class RoomManager extends Actor {
  import RoomManager._

  val rooms: Map[String, ActorRef] = readRooms()
  for (child <- context.children) child ! Room.LinkExits(rooms)

  def receive: Receive = {
    case BeginGame =>
      sender ! Player.StartRoom(rooms("Void"))
    case m => println("Unhandled message in RoomManager " + m)
  }

  def readRooms(): Map[String, ActorRef] = {
    val xmlData = xml.XML.loadFile("map.xml")
    (xmlData \ "room").map(readRoom).toMap
  }

  def readRoom(node: xml.Node): (String, ActorRef) = {
    val key = (node \ "@keyword").text
    val name = (node \ "@name").text
    val desc = (node \ "description").text
    val exits = (node \ "exits").text.split(",")
    val items = (node \ "item").map(n => Item((n \ "@name").text, n.text)).toList
    key -> context.actorOf(Props(new Room(name, desc, exits, items)), key)
  }

}

object RoomManager {
  case object BeginGame
}