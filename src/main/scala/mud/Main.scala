package mud

import akka.actor.{ActorSystem, Props}

import java.io.{BufferedReader, InputStreamReader, PrintStream}
import java.net.ServerSocket
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration.DurationInt

object Main {

  import PlayerManager._

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("MUDSystem")
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
    val roomManager = system.actorOf(Props[RoomManager], "RoomManager")
    system.scheduler.scheduleWithFixedDelay(0.seconds, 100.millis, playerManager, CheckInput)

    val ss = new ServerSocket(8080)

    while (true) {
      val sock = ss.accept()
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val out = new PrintStream(sock.getOutputStream)
      Future {
        out.println("Welcome to my MUD. What is your name?\n".trim())
        val name = Console.in.readLine()
        playerManager ! NewUser(name, in, out, sock, roomManager)
      }
    }

  }

}



