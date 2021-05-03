package mud

import akka.actor.{Actor, ActorRef}

class ActivityManager extends Actor {

  import ActivityManager._

  val activityPQ: PriorityQueue[Activity] = new PriorityQueue[Activity](_.delay < _.delay)
  private var numUpdates = 0

  def receive: Receive = {
    case CheckQueue =>
      numUpdates += 1
      while (!activityPQ.isEmpty && activityPQ.peek.delay <= numUpdates) {
        val nextActivity = activityPQ.dequeue()
        nextActivity.receiver ! nextActivity.msg
        activityPQ.dequeue()
        //        if (activityPQ.isEmpty) numUpdates = 0
      }
    case ScheduleActivity(task, sender, delay) => activityPQ.enqueue(Activity(task, sender, delay + numUpdates))
    case m => println("Unhandled message in ActivityManager " + m)
  }

}

object ActivityManager {

  case class ScheduleActivity(msg: Any, sender: ActorRef, delay: Int)

  case class Activity(msg: Any, receiver: ActorRef, delay: Int) //delay is number of updates

  case object CheckQueue

}

