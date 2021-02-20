package mud

import scala.io.StdIn._

class Player(val playerName: String = readLine("Welcome to my MUD. What is your name?\n").trim(),
             private var inventory: List[Item] = Nil) {


  //starting location as room 0
  private var currentLoc = Room.rooms("void_")

  println("*" * 100 + "\n" + currentLoc.wrap("You are finally awake " + playerName + ". My name is Sif, and I am the " +
    "onboard AI for this ship.  You have been in cryosleep for 5 Earth years since we left Earth. " +
    "Let me give you a summary of what happened. We left Earth in 2055 with our mission to reach Planet X. As soon as " +
    "we crossed Mars, you went into cryosleep just before our scheduled warp drive. Although the warp drive was " +
    "successful, it did not send us to Planet X, and now we are stuck at the outer edge of M32 galaxy. Where would you " +
    "like to warp drive now?\n") + "\n" + "*" * 100)
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
          case Some(item) => addToInventory(item)
        }
      case "drop" =>
        getFromInventory(subCommands(1)) match {
          case None => println(s"The ${subCommands(1)} item is not in your inventory")
          case Some(obtainedItem) => currentLoc.dropItem(obtainedItem); println(s"Dropped item ${obtainedItem.itemName}")
        }
      case c if c == "Inventory" || c == "inv" => println(inventoryListing())
      case c if "nsewup".contains(c.toLowerCase) || Set("north", "east", "south", "west", "up", "down")
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
    val goLoc = currentLoc.getExit("nsewud".indexOf(dir(0)))

    if (goLoc.isDefined) {
      currentLoc = goLoc.get
      currentLoc.fullDescription()
    } else println("Invalid exit")
  }

  //print a list of possible commands
  def printHelp(): Unit = println(
    """Sif supports the following commands:
'north', 'south', 'east', 'west', 'up', 'down' - for warp drive in a direction
'look' - get the full description of the current region
'inv'/'inventory' - list the contents of your spaceship's inventory
get 'item' - get an item from the planet and add it to your spaceship's inventory
drop 'item' -  drop an item from your spaceship's inventory into the void or the planet
exit - leave the game
help - print a list of commands and their description.""")

}