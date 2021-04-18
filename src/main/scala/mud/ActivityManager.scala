package mud

import akka.actor.Actor

class ActivityManager extends Actor {

  import ActivityManager._

  val activityPQ: PriorityQueue = ???

  def receive: Receive = {
    case CheckQueue => ???
    /*checks the queue and for any events that should have
     happened by now and sends back the message that was provided when it was scheduled */


  }

}

object ActivityManager {

  case object CheckQueue

}
