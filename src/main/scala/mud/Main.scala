package mud

import scala.io.StdIn._

object Main {
	def main(args: Array[String]): Unit = {
		var inputCommand: String = ""
		var player = new Player
		println(Room.rooms(0).desc)

		println(s"Welcome ${player.name}\nEnter 'help' to see list of commands")


		while (inputCommand!="exit") {
			inputCommand= readLine().trim()
			player.processCommand(inputCommand.toLowerCase)
		}

	}
}
