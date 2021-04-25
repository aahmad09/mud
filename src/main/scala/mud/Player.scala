package mud

import akka.actor.{Actor, ActorRef}

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
  private var move: Boolean = true
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
          out.println("Region does not exist")
      }
    case PickItem(oitem) =>
      oitem match {
        case Some(thing) =>
          addToInventory(thing)
          currentLoc ! Room.BroadcastInRoom(playerName, s"added ${thing.itemName} to their inventory")
        case None => out.println(s"This item is not in the planet")
      }
    case GetCurrentRoom =>
      sender ! currentLoc
    case GetTarget(tgt, weapon) =>
      Main.activityManager ! ActivityManager.ScheduleActivity(Attack(tgt, weapon), self, weapon.delay)
      tgt ! Player.InitiateAttack(playerName, weapon)
    case Attack(tgt, weapon) =>
      tgt ! Player.GotHit(playerName, weapon, currentLoc)
      move = false
    case InitiateAttack(charName, weapon) =>
      out.println(s"$charName has initiated an attack on you with their weapon ${weapon.itemName}, which has" +
        s"damage ${weapon.damage}\nOptions are to attack back or flee")
      move = false
    case GotHit(attacker, weapon, loc) =>
      if (loc == currentLoc) {
        out.println(s"$attacker attacked you with ${weapon.name} in ${loc.path.name}")
        hitPoints -= weapon.damage
        out.println(s"You took ${weapon.damage} damage. Health has now dropped to $hitPoints")
        if (hitPoints <= 0) {
          dead = true
          out.println("Game over!")
          sock.close()
        }
        sender ! Player.AttackOutcome(playerName, dead, hitPoints)
      } else {
        out.println(s"$playerName tried to attack you, but you fled just in time.")
      }

    case AttackOutcome(tgt: String, dead: Boolean, hitPoints: Int) =>
      if (dead) {
        out.println("You killed " + tgt)
        move = true
      } else out.println(s"$tgt survived attack, and their health is at $hitPoints")
    //    case GetStats =>
    //      sender ! stats
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
            currentLoc ! Room.BroadcastInRoom(playerName, s"dropped the item ${obtainedItem.itemName}")
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
            case Some(getItem) => if (subCommands(1).toLowerCase() == getItem.itemName) equippedItem = None
              out.println(s"Unequipped ${subCommands(1)}")
          }
        } catch {
          case e: ArrayIndexOutOfBoundsException => out.println("Please specify which item to unequip")
        }
      case "kill" =>
        equippedItem match {
          case None => out.println(s"You have not equipped any item")
          case Some(getItem) => currentLoc ! Room.FindCharacter(subCommands(1).split(" ")(0).toLowerCase(), getItem)
        }
      case "flee" =>
//        val dir = util.Random.nextInt(6)
        val dir = 2
        currentLoc ! Room.GetExit(dir)
      case c if c == "hp" || c == "health" =>
        out.println("Hitpoints: " + hitPoints)
      case c if c == "inventory" || c == "inv" =>
        out.println(inventoryListing())
      case c if "nsewup".contains(c.toLowerCase) || Set("north", "east", "south", "west", "up", "down").contains(c) =>
        move(command)
      case "say" => currentLoc ! Room.BroadcastInRoom(playerName, ": " + subCommands(1))
      case "tell" => val recieverAndMessage = subCommands(1).split(" ", 2)
        context.parent ! PlayerManager.PrivateMessage(self, recieverAndMessage(0), recieverAndMessage(1))
      case _ =>
        out.println(s"$command is not a valid command. Please re-enter.")
    }

  }

  //Pull an item out of the inventory if it exists and return it
  def getFromInventory(itemName: String): Option[Item] = {
    inventory.find(_.itemName.toLowerCase == itemName) match {
      case Some(item) =>
        inventory.remove(inventory.indexOf(item))
        Some(item)
      case None => None
    }
  }

  //Build a String with the contents of the inventory
  def inventoryListing(): String = {
    var invStr: String = "Inventory:\n"
    for (elem <- inventory) invStr += s"\t${elem.itemName} - ${elem.itemDesc}\n"
    if (invStr == "Inventory:\n") invStr = "Inventory: Empty "
    invStr.dropRight(1)
  }

  //Move the player in a particular direction if possible.
  def move(dir: String): Unit = {
    "nsewud".indexOf(dir.toLowerCase()(0)) match {
      case 0 => currentLoc ! Room.GetExit(0)
      case 1 => currentLoc ! Room.GetExit(1)
      case 2 => currentLoc ! Room.GetExit(2)
      case 3 => currentLoc ! Room.GetExit(3)
      case 4 => currentLoc ! Room.GetExit(4)
      case 5 => currentLoc ! Room.GetExit(5)
    }
  }

  //print a list of possible commands
  def printHelp(): String =
    """Sif supports the following commands:
'north', 'south', 'east', 'west', 'up', 'down' - for warp drive in a direction
'look' - get the full description of the current region
'inv'/'inventory' - list the contents of your spaceship's inventory
get 'item' - get an item from the planet and add it to your spaceship's inventory
drop 'item' -  drop an item from your spaceship's inventory into the void or the planet
exit - leave the game
help - print a list of commands and their description."""

  def stats: String = "_" * 40 + s"Player: $playerName\nHit points: $hitPoints" + "_" * 40

  //Add the given item to inventory
  def addToInventory(item: Item): Unit = {
    inventory += item
  }

  def stopGame(): Unit = {
    context.parent ! PlayerManager.RemovePlayer(playerName)
    currentLoc ! Room.RemoveCharacter(playerName, self)
    context.stop(self)
  }

}

object Player {

  case class PrintMessage(msg: String)

  case class GetTarget(tgt: ActorRef, weapon: Item)

  case class Attack(tgt: ActorRef, weapon: Item)

  case class GotHit(attacker: String, weapon: Item, loc: ActorRef)

  case class AttackOutcome(target: String, dead: Boolean, hitPoints: Int)

  case class TakeExit(oroom: Option[ActorRef])

  case class PickItem(oitem: Option[Item])

  case class Init(roomManager: ActorRef)

  case class StartRoom(room: ActorRef)

  case class MoveRoom(room: ActorRef)

  case class InitiateAttack(charName: String, weapon: Item)

  case object GetStats

  case object VerifyInput

  case object GetCurrentRoom

}



