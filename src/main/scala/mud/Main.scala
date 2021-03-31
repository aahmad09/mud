package mud

import akka.actor.{ActorSystem, Props}

import java.io.{BufferedReader, InputStreamReader, PrintStream}
import java.net.ServerSocket
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt


object Main {

  import PlayerManager._

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("MUDSystem")
    val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
    val roomManager = system.actorOf(Props[RoomManager], "RoomManager")
    system.scheduler.scheduleWithFixedDelay(0.seconds, 100.millis, playerManager, CheckInput)

    val ss = new ServerSocket(8080)
    println(s"... Server is running using port ${ss.getLocalPort} ...")

    while (true) {
      val sock = ss.accept()
      println("got player with IP: " + sock.getLocalAddress)
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val out = new PrintStream(sock.getOutputStream)
      Future {
        out.println("Welcome to my MUD. What is your name?\n".trim())
        val name = in.readLine()
        playerManager ! NewUser(name, in, out, sock, roomManager)
      }
    }

  }

}



