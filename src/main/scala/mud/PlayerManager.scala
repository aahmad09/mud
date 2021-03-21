package mud

import akka.actor.Actor

class PlayerManager extends Actor {

  def receive: Receive = {

    case m => println("Unhandled message in PlayerManager " + m)
  }

}
