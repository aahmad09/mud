package mud

import akka.actor.{Actor, ActorRef}

class NPC(val npcName: String, npcDesc: String, val npcWeapon: Item, private var currentLoc: ActorRef) extends Actor {

  import NPC._

  //try to move randomly every 10 seconds
  val moveDelay = 100

  private var hitPoints: Int = 80
  private var attackingState: Boolean = false
  private var dead = false
  private var canMove: Boolean = true

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
    case RndMove(dir: Int) =>
      if (!dead && canMove) currentLoc ! Room.GetExit(dir)
    case Player.GotHit(attackerRef, weapon, loc) =>
      if (loc == currentLoc) {
        hitPoints -= weapon.damage
        if (hitPoints <= 0) {
          dead = true
          context.parent ! NPCManager.RemoveNPC(npcName)
          currentLoc ! Room.RemoveCharacter(npcName, self)
        } else {
          if (!attackingState) {
            Main.activityManager ! ActivityManager
              .ScheduleActivity(Player.GotHit(self, npcWeapon, currentLoc), attackerRef, npcWeapon.delay)
            attackerRef ! Player.GetAttacked(npcName, npcWeapon)
            attackingState = true
          }
        }
        attackerRef ! Player.AttackOutcome(self, dead, hitPoints, weapon)
      }
    case Player.GetAttacked(_, _) =>
      canMove = false
    case Player.AttackOutcome(_, _, _, _) =>
      canMove = true
      attackingState = false
    case Player.PrintMessage(_) =>
      None
    case Player.ReturnStats(requester) =>
      requester ! Player.PrintMessage(s"NPC: $npcName\nDescription: $npcDesc\nWeapon: ${npcWeapon.name} - ${npcWeapon.desc}\nHit points: $hitPoints")
    case m => println("Unhandled message in NPC " + m)
  }

  def stopGame(): Unit = {
    context.parent ! PlayerManager.RemovePlayer(npcName)
    currentLoc ! Room.RemoveCharacter(npcName, self)
    context.stop(self)
  }

}

object NPC {

  case class Init(room: ActorRef)

  case class RndMove(dir: Int)

}
