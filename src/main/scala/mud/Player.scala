package mud

import akka.actor.{Actor, ActorRef}

import java.io.{BufferedReader, PrintStream}
import java.net.Socket

class Player(val playerName: String,
             in: BufferedReader, out: PrintStream, sock: Socket)
  extends Actor {

  import Player._

  //empty player inventory at start implemented as a mutable doubly linked list
  private val inventory: MutableDLList[Item] = new MutableDLList[Item]

  private var currentLoc: ActorRef = null

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
      currentLoc ! Room.AddPlayer(self)
      currentLoc ! Room.FullDescription
    case PrintMessage(msg) =>
      out.println(msg)
    case TakeExit(oroom) =>
      oroom match {
        case Some(pos) =>
          currentLoc ! Room.RemovePlayer(self)
          currentLoc ! Room.BroadcastInRoom(playerName, "exited this room")
          currentLoc = pos
          currentLoc ! Room.AddPlayer(self)
          currentLoc ! Room.FullDescription
          currentLoc ! Room.BroadcastInRoom(playerName, "entered this room")
        case None =>
          out.println("Invalid exit")
      }
    case PickItem(oitem) =>
      oitem match {
        case Some(thing) =>
          addToInventory(thing)
          currentLoc ! Room.BroadcastInRoom(playerName, s"added ${thing.itemName} to their inventory")
        case None => out.println(s"This item is not in the room")
      }
    case GetCurrentRoom =>
      sender ! currentLoc
    case m => out.println("Unhandled message in Player " + m)
  }

  //Parse and act on a command
  def processCommand(command: String): Unit = {
    val subCommands = command.split(" ", 2)
    subCommands(0).toLowerCase match {
      case "exit" =>
        out.println(s"Goodbye $playerName!")
        sock.close()
        context.stop(self)
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
      case c if c == "inventory" || c == "inv" =>
        out.println(inventoryListing())
      case c if "nsewup".contains(c.toLowerCase) || Set("north", "east", "south", "west", "up", "down").contains(c) =>
        move(command)
      case "say" => currentLoc ! Room.BroadcastInRoom(playerName, ": " + subCommands(1))
      case "tell" => val recieverAndMessage = subCommands(1).split(" ", 2)
        context.parent ! PlayerManager.PrivateMessage(self, recieverAndMessage(0), recieverAndMessage(1))
      case "exit" => context.parent ! PlayerManager.RemovePlayer(playerName)
        currentLoc ! Room.RemovePlayer(self)
        context.stop(self)
      case _ =>
        out.println(s"$command is not a valid command. Please re-enter.")
    }

  }

  //Pull an item out of the inventory if it exists and return it
  def getFromInventory(itemName: String): Option[Item] = {
    inventory.find(_.itemName.toLowerCase == itemName) match {
      case Some(item) =>
        inventory.remove(inventory.indexOf(item))
        //          inventory.patch(inventory.indexOf(item), Nil, 1)
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

  //Add the given item to inventory
  def addToInventory(item: Item): Unit = {
    inventory += item
  }

}

object Player {

  case class PrintMessage(msg: String)

  case class TakeExit(oroom: Option[ActorRef])

  case class PickItem(oitem: Option[Item])

  case class Init(roomManager: ActorRef)

  case class StartRoom(room: ActorRef)

  case object VerifyInput

  case object GetCurrentRoom

}

