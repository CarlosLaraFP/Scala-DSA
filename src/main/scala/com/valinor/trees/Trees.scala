package com.valinor.trees

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.{Queue => MutableQueue}

final class Node[A](v: Option[A] = None) {
  /*
    3 properties: value, children, parent
  */
  private var _value: Option[A] = v
  def value: Option[A] = _value
  def value_= (newValue: Option[A]): Unit = {
    _value = newValue
  }

  private var _left: Option[Node[A]] = None
  def left: Option[Node[A]] = _left
  def left_= (newValue: Node[A]): Unit = {
    _left = Some(newValue)
  }

  private var _right: Option[Node[A]] = None
  def right: Option[Node[A]] = _right
  def right_= (newValue: Node[A]): Unit = {
    _right = Some(newValue)
  }

  override def toString: String = _value.toString
}

class Tree[A](node: A) {
  /*
    Depth first search and breadth first search.
    Depth first search has 3 types: pre-order, in-order, and post-order.
  */
  protected var _root: Node[A] = new Node(Some(node))
  def root: Node[A] = _root
  def root_=(newValue: Node[A]): Unit = {
    _root = newValue
  }
}

final class INode[A](v: Option[A] = None) {
  /*
  */
  private var _value: Option[A] = v
  def value: Option[A] = _value
  def value_=(newValue: A): Unit = {
    _value = Some(newValue)
  }

  lazy val left: INode[A] = new INode[A]()
  def left_=(newValue: A): Unit = {
    left.value = newValue
  }

  lazy val right: INode[A] = new INode[A]()
  def right_=(newValue: A): Unit = {
    left.value = newValue
  }

  override def toString: String = _value.toString
}

class BinarySearchTree(value: Int) {
  /*
    Mutable implementation with a Queue
    O(log(N)) time complexity due to ordered structure
  */
  protected var _root: Node[Int] = new Node(Some(value))
  def root: Node[Int] = _root
  def root_=(newValue: Int): Unit = {
    _root = new Node(Some(newValue))
  }

  def insert(value: Int): Unit = {
    /*
      Let's assume that duplicates are overridden by the new node that is to be inserted.
      Other options are to keep a counter of duplicate nodes, or to keep a list of duplicates nodes with the same value.
      Time complexity: O(log(N))
    */
    @tailrec
    def insertHelper(node: Node[Int]): Unit = node.value match {
      case Some(x) if x > value && node.left.isDefined => insertHelper(node.left.get)
      case Some(x) if x > value && node.left.isEmpty => node.left = new Node(Some(value))
      case Some(x) if x < value && node.right.isDefined => insertHelper(node.right.get)
      case Some(x) if x < value && node.right.isEmpty => node.right = new Node(Some(value))
      case _ => node.value = Some(value) // duplicate case (override) & post-deletion fill
    }
    insertHelper(_root)
  }

  def search(value: Int): Option[Node[Int]] = {
    /*
      Define a search function that takes a value, and returns true if a node containing that value exists in the tree, otherwise false.
      Time complexity: O(log(N))
    */
    @tailrec
    def searchHelper(node: Node[Int]): Option[Node[Int]] = node.value match {
      case Some(x) if x > value && node.left.isDefined => searchHelper(node.left.get)
      case Some(x) if x > value && node.left.isEmpty => None
      case Some(x) if x < value && node.right.isDefined => searchHelper(node.right.get)
      case Some(x) if x < value && node.right.isEmpty => None
      case _ => Some(node)
    }
    searchHelper(_root)
  }

  def display: String = {
    /*
      Prints the tree structure in println levels with | separator
      This is a variation of Breadth First Search (BFS) with a mutable Queue
    */
    val queue = MutableQueue[(Option[Node[Int]], Int)]((Some(_root), 0))

    @tailrec
    def displayHelper(visitOrder: Vector[(Option[Int], Int)]): Vector[(Option[Int], Int)] = {
      if (queue.isEmpty) visitOrder
      else {
        val (node, level) = queue.dequeue
        if (node.isEmpty) displayHelper(visitOrder appended(None, level))
        else {
          if (node.get.left.isDefined) queue enqueue ((node.get.left, level + 1)) else queue enqueue ((None, level + 1))
          if (node.get.right.isDefined) queue enqueue ((node.get.right, level + 1)) else queue enqueue ((None, level + 1))
          displayHelper(visitOrder appended(node.get.value, level))
        }
      }
    }

    @tailrec
    def stringHelper(visitOrder: Vector[(Option[Int], Int)], previousLevel: Int, result: String): String = {
      if (visitOrder.isEmpty) result
      else {
        val (node, level) = visitOrder.head
        if (level == previousLevel) stringHelper(visitOrder.tail, previousLevel, result + " | " + node.toString)
        else stringHelper(visitOrder.tail, level, result + "\n" + node.toString)
      }
    }

    stringHelper(displayHelper(Vector()), -1, "Tree\n")
  }

  def delete(target: Int): Unit = {
    /*
      Binary Search Tree deletion algorithm
      Time complexity:
        1) Search is log(N) in the worst case
        2) Height (K) of the sub-branch is O(log(K))
      Conclusion: Worst time complexity stays O(log(N)) due to inverse relationship
      Space complexity (intermittent): O(log(N)) due to auxiliary Vector
    */
    def getBranchNodes(node: Option[Node[Int]], elements: Vector[Node[Int]], leftChoice: Boolean): Vector[Node[Int]] = node match {
      // DFS post-order traversal O(log(K))
      // This guarantees that the first element of the Vector is the largest sub-value
      case None => elements
      case Some(x) if leftChoice => getBranchNodes(x.right, elements, leftChoice) appended x
      case Some(x) if !leftChoice => getBranchNodes(x.left, elements, leftChoice) appended x
    }

    val node = search(target)

    if (node.isDefined && !node.contains(_root)) {
      if (node.get.left.isDefined) {
        val branch = getBranchNodes(node.get.left, Vector(), leftChoice = true).head
        node.get.value = branch.value
        branch.value = None
      }
      else if (node.get.right.isDefined) {
        val branch = getBranchNodes(node.get.right, Vector(), leftChoice = false).head
        node.get.value = branch.value
        branch.value = None
      }
      else node.get.value = None
    }
  }
}

class QueueTree[A](node: A) extends Tree[A](node: A) {
  /*
    This tree implementation is good for breadth-first search using FIFO access pattern.
  */
  private var _elements = Queue(node)
  def elements: Queue[A] = _elements

  def append(value: A): Unit = {
    /*
      Appending must fill out the level before filling in the next.
      Every append requires starting at the root and traversing until the first open spot: O(N)
    */
    _elements = _elements.enqueue(value)
  }
}

class StackTree[A](node: A) extends Tree[A](node: A) {

}

