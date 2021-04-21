package mud

import akka.actor.{Actor, ActorRef}

class ActivityManager extends Actor {

  import ActivityManager._

  val activityPQ: PriorityQueue[Activity] = new PriorityQueue[Activity](_.delay < _.delay)
  private var numUpdates = 0

  def receive: Receive = {
    //checks the queue for possible events. If so, return the command to receiver
    case CheckQueue =>
      if (!activityPQ.isEmpty) {
        numUpdates += 1
        var nextActivity = activityPQ.peek
        while (nextActivity.delay <= numUpdates) {
          nextActivity.receiver ! nextActivity.msg
          activityPQ.dequeue()
          if (!activityPQ.isEmpty) nextActivity = activityPQ.peek else numUpdates = 0
        }
      }
    case ScheduleActivity(task, sender, delay) => activityPQ.enqueue(Activity(task, sender, delay))
    case m => println("Unhandled message in ActivityManager " + m)
  }

}

object ActivityManager {

  case class ScheduleActivity(msg: Any, sender: ActorRef, delay: Int)

  case class Activity(msg: Any, receiver: ActorRef, delay: Int) //delay is number of updates

  case object CheckQueue

}

