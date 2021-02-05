package mud

import scala.io.StdIn._

class Player() {
  val name: String = readLine("Welcome to my MUD. What is your name?\n" ).trim()
  private var rooms$ = Room.rooms(0)

  def processCommand(command: String): Unit = {
    command match {
      case "exit" => println(s"Goodbye $name!")
      case "help" => printHelp()
      case "look" => println(rooms$.fullDescription())
      case _ => println(s"$command is not a valid command. Please re-enter.")
    }
  }
  def getFromInventory(itemName: String): Option[Item] = ???
  def addToInventory(item: Item): Unit = ???
  def inventoryListing(): String = ???
  def move(dir: String): Unit = ???
  def printHelp(): Unit = println("Some help message") //TODO Help message

}