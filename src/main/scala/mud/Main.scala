package mud

import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    var inputCommand: String = ""
    var player = new Player

    println(s"Welcome ${player.name}\nEnter 'help' to see a list of commands")


    while (inputCommand != "exit") {
      inputCommand = readLine().trim()
      player.processCommand(inputCommand.toLowerCase)
    }

  }
}
