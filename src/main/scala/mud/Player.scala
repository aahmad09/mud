package mud

import akka.actor.{Actor, ActorRef, PoisonPill}

import java.io.{BufferedReader, PrintStream}
import java.net.Socket

class Player(val playerName: String,
             in: BufferedReader, out: PrintStream, sock: Socket)
  extends Actor {

  import Player._

  private val inventory: MutableDLList[Item] = new MutableDLList[Item]
  private var hitPoints: Int = 50
  private var equippedItem: Option[Item] = None
  private var currentLoc: ActorRef = null
  private var canMove: Boolean = true
  private var dead: Boolean = false

  def receive: Receive = {
    case VerifyInput =>
      if (in.ready()) {
        processCommand(in.readLine())
      }
    case Init(roomManager) =>
      roomManager ! RoomManager.BeginGame
      out.println("You are finally awake " + playerName + ". My name is Sif, and I am the " +
        "onboard AI for this ship.  You have been in cryosleep for 5 Earth years since we left Earth. " +
        "Let me give you a summary of what happened. We left Earth in 2055 with our mission to reach Planet X. As soon as " +
        "we crossed Mars, you went into cryosleep just before our scheduled warp drive. Although the warp drive was " +
        "successful, it did not send us to Planet X, and now we are stuck at the outer edge of M32 galaxy. Where would you " +
        "like to warp drive now?\n")
      Thread.sleep(2000)
    case StartRoom(room: ActorRef) =>
      currentLoc = room
      currentLoc ! Room.AddCharacter(playerName, self)
      currentLoc ! Room.FullDescription
    case PrintMessage(msg) =>
      out.println(msg)
    case TakeExit(oroom) =>
      oroom match {
        case Some(pos) =>
          currentLoc ! Room.RemoveCharacter(playerName, self)
          currentLoc ! Room.BroadcastInRoom(playerName, "departed from this planet")
          currentLoc = pos
          currentLoc ! Room.AddCharacter(playerName, self)
          currentLoc ! Room.FullDescription
          currentLoc ! Room.BroadcastInRoom(playerName, "arrived at this planet")
        case None =>
          out.println("Cannot go that way")
      }
    case PickItem(oitem) =>
      oitem match {
        case Some(thing) =>
          addToInventory(thing)
          currentLoc ! Room.BroadcastInRoom(playerName, s"added ${thing.name} to their inventory")
        case None => out.println(s"This item is not in the planet")
      }
    case InitiateAttack(tgt, weapon) =>
      tgt match {
        case Some(tgtFound) => canMove = false
          Main.activityManager ! ActivityManager
            .ScheduleActivity(GotHit(self, weapon, currentLoc), tgtFound, weapon.delay)
          tgtFound ! Player.GetAttacked(playerName, weapon)
        case None => out.println("That character is not in the room")
      }
    case GetAttacked(charName, weapon) =>
      out.println(s"$charName has initiated an attack on you with their weapon ${weapon.name}, which has " +
        s"damage ${weapon.damage}\nYou have ${weapon.delay / 10} seconds to flee")
      canMove = false
    case GotHit(attackerRef, weapon, loc) =>
      if (loc == currentLoc) {
        out.println(s"${attackerRef.path.name} attacked you with ${weapon.name} in ${loc.path.name}")
        hitPoints -= weapon.damage
        out.println(s"You took ${weapon.damage} damage. Health has now dropped to $hitPoints")
        if (hitPoints <= 0) {
          dead = true
          out.println("Game over!")
          self ! PoisonPill
          sock.close()
        }
        attackerRef ! Player.AttackOutcome(self, dead, hitPoints, weapon)
      } else out.println(s"${attackerRef.path.name} tried to attack you, but you fled just in time.")
    case AttackOutcome(tgt, dead, hitPoints, weapon) =>
      if (dead) out.println("You killed " + tgt.path.name)
      else {
        Main.activityManager ! ActivityManager
          .ScheduleActivity(GotHit(self, weapon, currentLoc), tgt, weapon.delay)
        out.println(s"${tgt.path.name} survived your attack, and their health is at $hitPoints")
      }
      canMove = true
    case ReturnStats(requester) =>
      requester ! Player.PrintMessage(s"Health: $hitPoints\n${inventoryListing()}")
    case m => out.println("Unhandled message in Player " + m)
  }

  //Parse and act on a command
  def processCommand(command: String): Unit = {
    val subCommands = command.split(" ", 2)
    subCommands(0).toLowerCase match {
      case "exit" =>
        out.println(s"Goodbye $playerName!")
        sock.close()
        stopGame()
      case "help" =>
        self ! Player.PrintMessage(printHelp())
      case "look" =>
        currentLoc ! Room.FullDescription
      case "get" =>
        currentLoc ! Room.GetItem(subCommands(1).toLowerCase())
      case "drop" =>
        getFromInventory(subCommands(1).toLowerCase()) match {
          case None => out.println(s"The ${subCommands(1)} item is not in your inventory")
          case Some(obtainedItem) => currentLoc ! Room.DropItem(obtainedItem)
            inventory.remove(inventory.indexOf(obtainedItem))
            if (obtainedItem == equippedItem.get) equippedItem = None
            currentLoc ! Room.BroadcastInRoom(playerName, s"dropped the item ${obtainedItem.name}")
        }
      case "equip" =>
        getFromInventory(subCommands(1).toLowerCase()) match {
          case None => out.println(s"The ${subCommands(1)} item is not in your inventory")
          case Some(obtainedItem) => equippedItem = Option(obtainedItem)
            out.println(s"Equipped ${subCommands(1)}")
        }
      case "unequip" => //make better
        try {
          equippedItem match {
            case None => out.println(s"You have not equipped any item")
            case Some(getItem) => if (subCommands(1).toLowerCase() == getItem.name) equippedItem = None
              out.println(s"Unequipped ${subCommands(1)}")
          }
        } catch {
          case _: ArrayIndexOutOfBoundsException => out.println("Please specify which item to unequip")
        }
      case "kill" =>
        try {
          equippedItem match {
            case None => out.println(s"You have not equipped any item")
            case Some(getItem) => currentLoc ! Room.FindCharacter(subCommands(1).split(" ")(0), getItem)
          }
        } catch {
          case _: ArrayIndexOutOfBoundsException => out.println("please follow proper format of commands")
        }
      case "flee" =>
        val dir = util.Random.nextInt(6)
        currentLoc ! Room.GetExit(dir)
      case "inspect" => currentLoc ! Room.GetCharStats(subCommands(1).toLowerCase, self)
      case c if Set("stats", "statistics", "hp", "health").contains(c) =>
        out.println(s"Health: $hitPoints\n${inventoryListing()}")
      case c if c == "inventory" || c == "inv" =>
        out.println(inventoryListing())
      case c if "nsewud".contains(c.toLowerCase) || Set("north", "east", "south", "west", "up", "down").contains(c) =>
        if (canMove) move(command) else out.println("You cannot move because you are either attacking or being attacked.")
      case "say" => currentLoc ! Room.BroadcastInRoom(playerName, ": " + subCommands(1))
      case "tell" => val recieverAndMessage = subCommands(1).split(" ", 2)
        context.parent ! PlayerManager.PrivateMessage(self, recieverAndMessage(0), recieverAndMessage(1))
      case "shortestPath" => Main.roomManager ! RoomManager.ShortestPath(subCommands(1).toLowerCase)
      case "listRooms" => Main.roomManager ! RoomManager.GetRooms
      case _ =>
        out.println(s"$command is not a valid command. Please re-enter.")
    }
  }

  //Pull an item out of the inventory if it exists and return it
  def getFromInventory(itemName: String): Option[Item] = {
    inventory.find(_.name.toLowerCase == itemName) match {
      case Some(item) => Some(item)
      case None => None
    }
  }

  //Move the player in a particular direction if possible.
  def move(dir: String): Unit =
    "nsewud".indexOf(dir.toLowerCase()(0)) match {
      case 0 => currentLoc ! Room.GetExit(0)
      case 1 => currentLoc ! Room.GetExit(1)
      case 2 => currentLoc ! Room.GetExit(2)
      case 3 => currentLoc ! Room.GetExit(3)
      case 4 => currentLoc ! Room.GetExit(4)
      case 5 => currentLoc ! Room.GetExit(5)
    }

  //print a list of possible commands
  def printHelp(): String =
    """Sif supports the following commands:
'north', 'south', 'east', 'west', 'up', 'down' - for warp drive in a direction
'look' - get the full description of the current region
'inv'/'inventory' - list the contents of your spaceship's inventory
get 'item' - get an item from the planet and add it to your spaceship's inventory
drop 'item' -  drop an item from your spaceship's inventory into the void or the planet
equip 'item' - equip an item that is in your inventory
unequip 'item' - unequip an item that is currently equipped
kill 'character' 'item' - kill another character using an item that you have equipped
inspect 'character' - get another player or NPC stats
'stats' - get your own stats
'say' - broadcast a message in your current room
'tell' - whisper a message to another player
exit - leave the game
help - print a list of commands and their description."""

  def stopGame(): Unit = {
    context.parent ! PlayerManager.RemovePlayer(playerName)
    currentLoc ! Room.RemoveCharacter(playerName, self)
    context.stop(self)
  }

  //Build a String with the contents of the inventory
  def inventoryListing(): String = {
    var invStr: String = "Inventory:\n"
    for (elem <- inventory) if (elem != equippedItem.get) invStr += s"\t${elem.name} - ${elem.desc}\n"
    if (invStr == "Inventory:\n") invStr = "Inventory: Empty "
    invStr = invStr.dropRight(1)
    //do a foreach on a list if multiple items can be equipped
    equippedItem match {
      case Some(item) => invStr += s"\nEquipped Items: \n\t${item.name} - ${item.desc}"
      case None => invStr += "\nEquipped Items: None"
    }
    invStr
  }

  //Add the given item to inventory
  def addToInventory(item: Item): Unit = {
    inventory += item
  }

}

object Player {

  case class PrintMessage(msg: String)

  case class InitiateAttack(tgt: Option[ActorRef], weapon: Item)

  case class GotHit(attackerRef: ActorRef, weapon: Item, loc: ActorRef)

  case class AttackOutcome(target: ActorRef, dead: Boolean, hitPoints: Int, weapon: Item)

  case class TakeExit(oroom: Option[ActorRef])

  case class PickItem(oitem: Option[Item])

  case class Init(roomManager: ActorRef)

  case class StartRoom(room: ActorRef)

  case class MoveRoom(room: ActorRef)

  case class GetAttacked(charName: String, weapon: Item)

  case class ReturnStats(userRef: ActorRef)

  case object VerifyInput

}


