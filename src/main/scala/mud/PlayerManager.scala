package mud

import akka.actor.{Actor, ActorRef, Props}
import mud.Player.GetCurrentRoom

import java.io.{BufferedReader, PrintStream}
import java.net.Socket

class PlayerManager extends Actor {

  import PlayerManager._

  private var playersMap = Map[String, ActorRef]()

  def receive: Receive = {
    case CheckInput =>
      context.children.foreach(child => child ! Player.VerifyInput)
    case NewUser(name, in, out, sock, roomManager) =>
      if (context.children.exists(_.path.name == name)) {
        out.println("Sorry, that username already exists!")
        sock.close()
      } else {
        val newPlayer = context.actorOf(Props(new Player(name, in, out, sock)), name)
        newPlayer ! Player.Init(roomManager)
        playersMap += name -> newPlayer
      }
    case PrivateMessage(sender, receiver,  msg) =>
      playersMap(receiver) ! Player.PrintMessage(sender.path.name + " whispered " + msg)
    case LocatePlayer(player) => //TODO: implement correctly
      if (playersMap.contains(player)) sender ! Player.PrintMessage((playersMap(player) ! GetCurrentRoom).toString)
    case m => println("Unhandled message in PlayerManager " + m)
  }

}

object PlayerManager {

  case class NewUser(name: String, in: BufferedReader, out: PrintStream, sock: Socket, roomManager: ActorRef)

  case object Init

  case object CheckInput

  case class PrivateMessage(sender: ActorRef, receiver: String, msg: String)

  case class LocatePlayer(player: String)

}

