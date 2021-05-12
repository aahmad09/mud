package mud

import scala.reflect.ClassTag

class BinaryHeap[A: ClassTag](higherP: (A, A) => Boolean) {
  private var heap = Array.fill(10)(null.asInstanceOf[A])
  private var end = 1

  def enqueue(a: A): Unit = {
    if (end >= heap.length) {
      val tmp = Array.fill(heap.length * 2)(null.asInstanceOf[A])
      for (i <- heap.indices) tmp(i) = heap(i)
      heap = tmp
    }
    var bubble = end
    while (bubble > 1 && higherP(a, heap(bubble / 2))) {
      heap(bubble) = heap(bubble / 2)
      bubble /= 2
    }
    heap(bubble) = a
    end += 1
  }

  def dequeue(): A = {
    val ret = heap(1)
    end -= 1
    val tmp = heap(end)
    var stone = 1
    var flag = true
    while (stone * 2 < end && flag) {
      var higherPChild = stone * 2
      if (higherPChild + 1 < end && higherP(heap(higherPChild + 1), heap(higherPChild))) higherPChild += 1
      if (higherP(heap(higherPChild), tmp)) {
        heap(stone) = heap(higherPChild)
        stone = higherPChild
      } else flag = false
    }
    heap(stone) = tmp
    ret
  }

  def peek: A = heap(1)

  def isEmpty: Boolean = end == 1
}