package mud

import akka.actor.{Actor, ActorRef}

class NPC(val npcName: String, private var currentLoc: ActorRef) extends Actor {

  import NPC._

  val moveDelay = 10

  def receive: Receive = {
    case Init =>
      currentLoc ! Room.AddCharacter(self)
      Main.activityManager ! ActivityManager.ScheduleActivity(RndMove(util.Random.nextInt(6)), self, moveDelay)
    case TakeExit(oroom) =>
      oroom match {
        case Some(pos) =>
          currentLoc ! Room.RemoveCharacter(self)
          currentLoc ! Room.BroadcastInRoom(npcName, "departed from this planet")
          currentLoc = pos
          currentLoc ! Room.AddCharacter(self)
          currentLoc ! Room.BroadcastInRoom(npcName, "arrived at this planet")
          Main.activityManager ! ActivityManager.ScheduleActivity(RndMove(util.Random.nextInt(6)), self, moveDelay)
        case None => Main.activityManager ! ActivityManager.ScheduleActivity(RndMove(util.Random.nextInt(6)), self, moveDelay)
      }
    case RndMove(dir: Int) =>
      currentLoc ! Room.GetNPCExit(dir)
    case PrintMessage(msg) => None
    case m => println("Unhandled message in NPC " + m)
  }

}

object NPC {

  case class Init(room: ActorRef)

  case class TakeExit(oroom: Option[ActorRef])

  case class RndMove(dir: Int)

  case class PrintMessage(msg: String)

}
