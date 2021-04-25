package mud

import akka.actor.{Actor, ActorRef}

class NPC(val npcName: String, npcDesc: String, private var currentLoc: ActorRef) extends Actor {

  import NPC._

  //try to move randomly every 10 seconds
  val moveDelay = 100

  private var hitPoints: Int = 10
  private var dead = false

  def receive: Receive = {
    case Init =>
      currentLoc ! Room.AddCharacter(npcName, self)
      Main.activityManager ! ActivityManager.ScheduleActivity(RndMove(util.Random.nextInt(6)), self, moveDelay)
    case Player.TakeExit(oroom) =>
      oroom match {
        case Some(pos) =>
          currentLoc ! Room.RemoveCharacter(npcName, self)
          currentLoc ! Room.BroadcastInRoom(npcName, "departed from this planet")
          currentLoc = pos
          currentLoc ! Room.AddCharacter(npcName, self)
          currentLoc ! Room.BroadcastInRoom(npcName, "arrived at this planet")
          Main.activityManager ! ActivityManager.ScheduleActivity(RndMove(util.Random.nextInt(6)), self, moveDelay)
        case None =>
          Main.activityManager ! ActivityManager.ScheduleActivity(RndMove(util.Random.nextInt(6)), self, moveDelay)
      }
    //    case Player.GetStats =>
    //      sender ! Room.SendStats
    case RndMove(dir: Int) =>
      currentLoc ! Room.GetExit(dir)
    case Player.GotHit(attacker: String, weapon: Item, loc: ActorRef) =>
      hitPoints -= weapon.damage
      if (hitPoints <= 0) {
        dead = true
        sender ! Player.AttackOutcome(npcName, dead, hitPoints)
      } else {
        sender ! Player.AttackOutcome(npcName, dead, hitPoints)
        loc ! Room.FindCharacter(attacker, None.asInstanceOf[Item])
      }
      println(npcName + " " + hitPoints) //TODO: remove
      sender ! Player.AttackOutcome(npcName, dead, hitPoints)
    case Player.AttackOutcome(name, dead, hitPoints) =>
      if (!dead) currentLoc ! Room.FindCharacter(name, None.asInstanceOf[Item])
    case Player.PrintMessage(_) =>
      None
    case m => println("Unhandled message in NPC " + m)
  }

  def stats: String = "_" * 40 + s"NPC: $npcName\nDescription: $npcDesc\nHit points: $hitPoints" + "_" * 40

}

object NPC {

  case class Init(room: ActorRef)

  case class RndMove(dir: Int)

}
