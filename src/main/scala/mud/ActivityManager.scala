package mud

import akka.actor.{Actor, ActorRef}

class ActivityManager extends Actor {

  import ActivityManager._

  val activityPQ: BinaryHeap[Activity] = new BinaryHeap[Activity](_.delay < _.delay)
  private var numUpdates = 0

  def receive: Receive = {
    case CheckQueue =>
      numUpdates += 1
      while (!activityPQ.isEmpty && activityPQ.peek.delay <= numUpdates) {
        val nextActivity = activityPQ.peek
        nextActivity.receiver ! nextActivity.msg
        activityPQ.dequeue()
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

