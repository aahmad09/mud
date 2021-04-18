package mud

import akka.actor.Actor

class NPC extends Actor {

  import NPC._

  def receive: Receive = {
    case CreateNPC => ???

  }

}

object NPC {

  case object CreateNPC

}
