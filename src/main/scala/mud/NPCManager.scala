package mud

import akka.actor.{Actor, ActorRef, Props}

class NPCManager extends Actor {

  import NPCManager._

  private var npcMap: Map[String, ActorRef] = Map[String, ActorRef]()

  def receive: Receive = {
    case CreateNPC(name, desc, weapon, location) =>
      val newNPC = context.actorOf(Props(new NPC(name, desc, weapon, location)), name)
      newNPC ! NPC.Init
      npcMap += name -> newNPC
      context.actorOf(Props(new NPC(name, desc, weapon, location)))
    case RemoveNPC(npcName) =>
      npcMap - npcName
    case PlayerManager.RemovePlayer(npc) =>
      npcMap - npc
    case m => println("Unhandled message in NPCManager " + m)
  }
}

object NPCManager {

  case class CreateNPC(name: String, desc: String, weapon: Item, location: ActorRef)

  case class RemoveNPC(npcName: String)

  case object MoveNPCs

}

