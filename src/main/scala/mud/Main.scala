package mud

import akka.actor.{ActorSystem, Props}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContextExecutor, Future}

object Main {

  import PlayerManager._

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("MUDSystem")
    val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
    val roomManager = system.actorOf(Props[RoomManager], "RoomManager")
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val in = Console.in
    val out = Console.out

    Future {
      out.println("Welcome to my MUD. What is your name?\n".trim())
      val name = in.readLine()
      playerManager ! NewUser(name, in, out, roomManager)
    }
    system.scheduler.scheduleWithFixedDelay(0.seconds, 100.millis, playerManager, CheckInput)

  }

}