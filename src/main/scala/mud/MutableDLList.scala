package mud

import scala.collection.mutable

class MutableDLList[A] extends mutable.Buffer[A] {
  private var default: A = _
  private val sentinel = new Node(default, null, null)
  private var numElems = 0

  sentinel.next = sentinel
  sentinel.prev = sentinel

  def +=:(elem: A): MutableDLList.this.type = {
    val newNode = new Node(elem, sentinel, sentinel.next)
    sentinel.next.prev = newNode
    sentinel.next = newNode
    numElems += 1
    this
  }


  def apply(n: Int): A = {
    if (n < 0 || n >= numElems) throw new IndexOutOfBoundsException(n + " of " + numElems)
    var rover = sentinel.next
    for (i <- 0 until n) rover = rover.next
    rover.data
  }

  def clear(): Unit = {
    sentinel.prev = sentinel
    sentinel.next = sentinel
    numElems = 0
  }

  def insertAll(n: Int, elems: Traversable[A]): Unit = {
    if (n < 0 || n >= numElems + 1) throw new IndexOutOfBoundsException(n + " of " + numElems)
    if (elems.nonEmpty) {
      var rover = sentinel.next
      for (i <- 0 until n) rover = rover.next
      for (e <- elems) {
        val newNode = new Node(e, rover.prev, rover)
        rover.prev.next = newNode
        rover.prev = newNode
        numElems += 1
      }
    }
  }

  def iterator: Iterator[A] = new Iterator[A] {
    var rover: Node = sentinel.next

    def hasNext: Boolean = rover != sentinel

    def next: A = {
      val ret = rover.data
      rover = rover.next
      ret
    }
  }

  def length: Int = numElems

  def remove(n: Int): A = {
    if (n < 0 || n >= numElems) throw new IndexOutOfBoundsException(n + " of " + numElems)
    numElems -= 1
    var rover = sentinel.next
    for (i <- 0 until n) rover = rover.next
    val ret = rover.data
    rover.prev.next = rover.next
    rover.next.prev = rover.prev
    ret
  }

  def update(n: Int, newelem: A) {
    if (n < 0 || n >= numElems) throw new IndexOutOfBoundsException(n + " of " + numElems)
    var rover = sentinel.next
    for (i <- 0 until n) rover = rover.next
    rover.data = newelem
  }

  override def foreach[U](f: A => U): Unit = {
    var rover = sentinel.next
    while (rover != sentinel) {
      f(rover.data)
      rover = rover.next
    }
  }

  override def filter(pred: A => Boolean): MutableDLList[A] = {
    val ret = new MutableDLList[A]()
    var rover = sentinel.next
    while (rover != sentinel) {
      if (pred(rover.data)) ret += rover.data
      rover = rover.next
    }
    ret
  }

  def myMap[B](f: A => B): MutableDLList[B] = {
    val ret = new MutableDLList[B]()
    var rover = sentinel.next
    while (rover != sentinel) {
      ret += f(rover.data)
      rover = rover.next
    }
    ret
  }

  def +=(elem: A): MutableDLList.this.type = {
    val newNode = new Node(elem, sentinel.prev, sentinel)
    sentinel.prev.next = newNode
    sentinel.prev = newNode
    numElems += 1
    this
  }

  override def toString: String = mkString("MutableDLList(", ", ", ")")

  private class Node(var data: A, var prev: Node, var next: Node)
}