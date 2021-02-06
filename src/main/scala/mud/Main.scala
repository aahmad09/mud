package mud

import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    var inputCommand: String = ""

    val player = new Player

    while (inputCommand != "exit") {
      inputCommand = readLine().trim()
      player.processCommand(inputCommand.toLowerCase)
    }

  }
}
