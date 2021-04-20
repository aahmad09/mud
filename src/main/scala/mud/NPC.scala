package mud

import akka.actor.{Actor, ActorRef}

class NPC(val npcName: String, private var currentLoc: ActorRef) extends Actor {

  import NPC._

  def receive: Receive = {
    case Init(room) =>
      currentLoc = room
      currentLoc ! Room.AddPlayer(self)
    case m => println("Unhandled message in NPC " + m)
  }

}

object NPC {

  case class Init(room: ActorRef)

}
