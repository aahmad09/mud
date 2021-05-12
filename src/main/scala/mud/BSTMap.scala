package mud

import scala.annotation.tailrec
import scala.collection.mutable

class BSTMap[K, V](lt: (K, K) => Boolean) extends mutable.Map[K, V] {
  private var root: Node = null

  def -=(key: K): BSTMap.this.type = {
    def findVictim(n: Node): Node = {
      if (n == null) n
      else if (n.key == key) {
        if (n.left == null) n.right
        else if (n.right == null) n.left
        else {
          val (link, max) = findRemoveMax(n.left)
          max.right = n.right
          max.left = link
          max
        }
      } else if (lt(key, n.key)) {
        n.left = findVictim(n.left)
        n
      } else {
        n.right = findVictim(n.right)
        n
      }
    }

    /**
     * Returns a tuple of the node to link to and the max ndoe that was found.
     * Should never be called with null.
     */
    def findRemoveMax(n: Node): (Node, Node) = {
      if (n.right == null) {
        n.left -> n
      } else {
        val (link, max) = findRemoveMax(n.right)
        n.right = link
        (n, max)
      }
    }

    root = findVictim(root)
    this
  }

  def +=(kv: (K, V)): BSTMap.this.type = {
    def helper(n: Node): Node = {
      if (n == null) new Node(kv._1, kv._2, null, null)
      else {
        if (n.key == kv._1) {
          n.value = kv._2
        } else if (lt(kv._1, n.key)) {
          n.left = helper(n.left)
        } else {
          n.right = helper(n.right)
        }
        n
      }
    }

    root = helper(root)
    this
  }

  def countLeaf(n: Node): Int = {
    if (n == null) 0
    else if (n.left == null && n.right == null) 1
    else countLeaf(n.left) + countLeaf(n.right)
  }

  def maxDepth(n: Node): Int = {
    if (n == null) 0
    else Math.max(maxDepth(n.left), maxDepth(n.right)) + 1
  }

  def countNodes(n: Node): Int = {
    if (n == null) 0
    else countNodes(n.left) + countNodes(n.right)
  }

  // Members declared in scala.collection.MapLike
  def get(key: K): Option[V] = {
    var rover = root
    while (rover != null && rover.key != key) {
      if (lt(key, rover.key)) rover = rover.left
      else rover = rover.right
    }
    if (rover == null) None else Some(rover.value)
  }

  def iterator = new Iterator[(K, V)] {
    val stack = collection.mutable.Stack[Node]()

    @tailrec
    def pushAllLeft(n: Node): Unit = {
      if (n != null) {
        stack.push(n)
        pushAllLeft(n.left)
      }
    }

    pushAllLeft(root)

    def hasNext: Boolean = stack.nonEmpty

    def next(): (K, V) = {
      val ret = stack.pop()
      pushAllLeft(ret.right)
      ret.key -> ret.value
    }
  }

  def preorder(visit: (K, V) => Unit): Unit = {
    def helper(n: Node): Unit = if (n != null) {
      visit(n.key, n.value)
      helper(n.left)
      helper(n.right)
    }

    helper(root)
  }

  def postorder(visit: (K, V) => Unit): Unit = {
    def helper(n: Node): Unit = if (n != null) {
      helper(n.left)
      helper(n.right)
      visit(n.key, n.value)
    }

    helper(root)
  }

  def inorder(visit: (K, V) => Unit): Unit = {
    def helper(n: Node): Unit = if (n != null) {
      helper(n.left)
      visit(n.key, n.value)
      helper(n.right)
    }

    helper(root)
  }

  private class Node(val key: K, var value: V, var left: Node, var right: Node)
}
