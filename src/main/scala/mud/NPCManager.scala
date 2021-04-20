package mud

import akka.actor.{Actor, ActorRef, Props}

class NPCManager extends Actor {

  import NPCManager._

  private var npcMap: Map[String, ActorRef] = Map[String, ActorRef]()

  def receive: Receive = {
    case CreateNPC(name, location) =>
      val newNPC = context.actorOf(Props(new NPC(name, location)), name)
      npcMap += name -> newNPC
    case m => println("Unhandled message in NPCManager " + m)
  }
}

object NPCManager {

  case class CreateNPC(name: String, location: ActorRef)

}
