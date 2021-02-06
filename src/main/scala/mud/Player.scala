package mud

import scala.io.StdIn._

class Player() {
  val name: String = readLine("Welcome to my MUD. What is your name?\n").trim()
  private val startingLoc = Room.rooms(0)
  private var currentLoc = startingLoc
  private var inventory: List[Item] = Nil


  def processCommand(command: String): Unit = {
    command match {
      case "exit" => println(s"Goodbye $name!")
      case "help" => printHelp()
      case "look" => currentLoc.fullDescription()
      case c if c.take(3) == "get" => addToInventory(currentLoc.getItem(command.substring(4)).get)
      case c if c.take(3) == "inv" => println(inventoryListing())
      case _ => println(s"$command is not a valid command. Please re-enter.")
    }
  }

  def printHelp(): Unit = println("Some help message") //TODO Help message

  def getFromInventory(itemName: String): Option[Item] = ???

  def addToInventory(item: Item): Unit = ???

  def inventoryListing(): String = {
    "Inventory:\n\t"
    //TODO Item1 - item1 description
  }



  def move(dir: String): Unit = dir.take(1).toLowerCase match {
    case "n" => currentLoc.getExit(0)
    case "s" => currentLoc.getExit(1)
    case "e" => currentLoc.getExit(2)
    case "w" => currentLoc.getExit(3)
    case "u" => currentLoc.getExit(4)
    case "d" => currentLoc.getExit(5)
  }

}