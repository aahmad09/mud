package mud

import scala.io.StdIn._

class Player(val playerName: String = readLine("Welcome to my MUD. What is your name?\n").trim(),
             private var inventory: List[Item] = Nil) {

  println(s"Welcome $playerName\nEnter 'help' to see a list of commands")

  //starting location as room 0
  private var currentLoc = Room.rooms(0)
  currentLoc.fullDescription()

  //Parse and act on a command
  def processCommand(command: String): Unit = {
    val subCommands = command.split(" ")
    subCommands(0) match {
      case "exit" => println(s"Goodbye $playerName!")
      case "help" => printHelp()
      case "look" => currentLoc.fullDescription()
      case "get" =>
        currentLoc.getItem(subCommands(1)) match {
          case None => println(s"The item ${subCommands(1)} is not in the room")
          case Some(item) =>  addToInventory(item)
        }
      case "drop" =>
        getFromInventory(subCommands(1)) match {
          case None => println(s"The ${subCommands(1)} item is not in your inventory")
          case Some(obtainedItem) =>  currentLoc.dropItem(obtainedItem)
        }
      case c if c == "Inventory" || c == "inv" => println(inventoryListing())
      case c if "nsewup".contains(c.toLowerCase) || Array("north", "east", "south", "west", "up", "down")
        .contains(c.toLowerCase) => move(command)
      case _ => println(s"$command is not a valid command. Please re-enter.")
    }
  }

  //Pull an item out of the inventory if it exists and return it
  def getFromInventory(itemName: String): Option[Item] = {
    inventory.find(_.itemName.toLowerCase == itemName) match {
      case Some(item) =>
        inventory = inventory.patch(inventory.indexOf(item), Nil, 1)
        Some(item)
      case None => None
    }
  }

  //Add the given item to inventory
  def addToInventory(item: Item): Unit = {
    inventory = item :: inventory
    println(s"Added ${item.itemName} to inventory")
  }

  //Build a String with the contents of the inventory
  def inventoryListing(): String = {
    var invStr: String = "Inventory:\n"
    for (elem <- inventory) invStr += s"\t${elem.itemName} - ${elem.itemDesc}\n"
    if (invStr == "Inventory:\n") invStr = "Inventory: Empty"
    invStr
  }

  //Move the player in a particular direction if possible.
  def move(dir: String): Unit = {
    var goLoc: Option[Room] = None

    dir.take(1).toLowerCase match {
      case "n" => goLoc = currentLoc.getExit(0)
      case "s" => goLoc = currentLoc.getExit(1)
      case "e" => goLoc = currentLoc.getExit(2)
      case "w" => goLoc = currentLoc.getExit(3)
      case "u" => goLoc = currentLoc.getExit(4)
      case "d" => goLoc = currentLoc.getExit(5)
    }

    if (goLoc.isDefined) {
      currentLoc = goLoc.get
      currentLoc.fullDescription()
    } else {
      println("Invalid exit")
    }

  }

  //print a list of possible commands
  def printHelp(): Unit = println("""The following commands are supported:
'north', 'south', 'east', 'west', 'up', 'down' - for movement
'look' - get the full description of the current room
'inv'/'inventory' - list the contents of your inventory
get 'item' - get an item from the room and add it to your inventory
drop 'item' -  drop an item from your inventory into the room
exit - leave the game
help - print a list of commands and their description.""")

}