package mud

//sorted pq
class PriorityQueue[A](comp: (A, A) => Boolean) {
  private var default: A = _
  private val end = new Node(default, null, null)

  def enqueue(obj: A): Unit = {
    var rover = end.prev
    while (rover != end && comp(obj, rover.data)) rover = rover.prev
    rover.next.prev = new Node(obj, rover, rover.next)
    rover.next = rover.next.prev
  }

  end.next = end
  end.prev = end

  def dequeue(): A = {
    val ret = end.next.data
    end.next = end.next.next
    end.next.prev = end
    ret
  }

  def peek: A = end.next.data

  def isEmpty: Boolean = end.next == end

  def removeMatches(f: A => Boolean): Unit = {
    var rover = end.next
    while (rover != end) {
      if (f(rover.data)) {
        rover.prev.next = rover.next
        rover.next.prev = rover.prev
      }
      rover = rover.next
    }
  }

  private class Node(var data: A, var prev: Node, var next: Node)

}