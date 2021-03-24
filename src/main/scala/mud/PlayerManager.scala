package mud

import akka.actor.{Actor, ActorRef, Props}

import java.io.{BufferedReader, PrintStream}
import java.net.Socket

class PlayerManager extends Actor {

  import PlayerManager._

  def receive: Receive = {
    case CheckInput =>
      context.children.foreach(child => child ! Player.VerifyInput)
    case NewUser(name, in, out, sock, roomManager) =>
      println("no") //TODO: rem
      if (context.children.exists(_.path.name == name)) {
        Console.out.println("Sorry, that username already exists!")
      } else {
        val newPlayer = context.actorOf(Props(new Player(name, in, out, sock)), name)
        Console.out.println("...")
        newPlayer ! Player.Init(roomManager)
      }
    case m => println("Unhandled message in PlayerManager " + m)
  }

}

object PlayerManager {

  case class NewUser(name: String, in: BufferedReader, out: PrintStream, sock: Socket, roomManager: ActorRef)

  case object CheckInput

  case object Init

}