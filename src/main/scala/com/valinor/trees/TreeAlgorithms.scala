package com.valinor.trees

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.{Queue => MutableQueue}
import scala.collection.mutable.{Map => MutableMap}

object TreeAlgorithms extends App {
  /*
    Main trees takeaway: Use the tree implementation that best suits your access pattern.
    In practice, any Tree implementation should be balanced: Add/remove elements in a consistent way (Stack, Queue, IndexedSeq).
    This makes DFS and BFS methods much more efficient.
  */
  def dfsPreOrderTraversal[A](root: Node[A]): Vector[A] = {
    /*
      DFS Pre-Order Traversal
      Worst time complexity: O(N)
    */
    //@tailrec
    def traverseHelper(node: Option[Node[A]], visitOrder: Vector[A]): Vector[A] = node match {
      case Some(x) =>
        val left = traverseHelper(x.left, visitOrder appended x.value.get)
        traverseHelper(x.right, left)
      case _ => visitOrder
    }
    traverseHelper(Some(root), Vector[A]())
  }

  def dfsInOrderTraversal[A](root: Node[A]): Vector[A] = {
    /*
      DFS In Order Traversal
      Worst time complexity: O(N)
      "D", "B", "H", "A", "C"
    */
    def traversalHelper(node: Option[Node[A]], visitOrder: Vector[A]): Vector[A] = node match {
      case Some(x) =>
        val left = traversalHelper(x.left, visitOrder)
        traversalHelper(x.right, left appended x.value.get)
      case _ => visitOrder
    }
    traversalHelper(Some(root), Vector())
  }

  def dfsPostOrderTraversal[A](root: Node[A]): Vector[A] = {
    /*
      DFS Post-Order Traversal
      Worst time complexity: O(N)
      We cannot check off a Node until we have checked off all its children, starting at the bottom.
    */
    def traversalHelper(node: Option[Node[A]], visitOrder: Vector[A]): Vector[A] = node match {
      case Some(x) =>
        val left = traversalHelper(x.left, visitOrder)
        val right = traversalHelper(x.right, left)
        traversalHelper(None, right appended x.value.get)
      case _ => visitOrder
    }
    traversalHelper(Some(root), Vector())
  }

  def bfsTraversal[A](root: Node[A]): Vector[A] = {
    /*
      BFS traversal algorithm
      Worst time complexity: O(N)
      "A", "B", "C", "D", "H", "S", "M"
    */
    val queue = MutableQueue(root)
    @tailrec
    def traversalHelper(visitOrder: Vector[A]): Vector[A] = {
      if (queue.isEmpty) visitOrder
      else {
        val node = queue.dequeue
        if (node.left.isDefined) queue enqueue node.left.get
        if (node.right.isDefined) queue enqueue node.right.get
        traversalHelper(visitOrder appended node.value.get)
      }
    }
    traversalHelper(Vector())
  }

  def display[A](root: Node[A]): String = {
    /*
      Prints the tree structure in println levels with | separator
      This is a variation of Breadth First Search (BFS) with a mutable Queue
    */
    val queue = MutableQueue[(Option[Node[A]], Int)]((Some(root), 0))
    @tailrec
    def displayHelper(visitOrder: Vector[(Option[A], Int)]): Vector[(Option[A], Int)] = {
      if (queue.isEmpty) visitOrder
      else {
        val (node, level) = queue.dequeue
        if (node.isEmpty) displayHelper(visitOrder appended (None, level))
        else {
          if (node.get.left.isDefined) queue enqueue ((node.get.left, level + 1)) else queue enqueue ((None, level + 1))
          if (node.get.right.isDefined) queue enqueue ((node.get.right, level + 1)) else queue enqueue ((None, level + 1))
          displayHelper(visitOrder appended (node.get.value, level))
        }
      }
    }
    @tailrec
    def stringHelper(visitOrder: Vector[(Option[A], Int)], previousLevel: Int, result: String): String = {
      if (visitOrder.isEmpty) result
      else {
        val (node, level) = visitOrder.head
        if (level == previousLevel) stringHelper(visitOrder.tail, previousLevel, result + " | " + node.toString)
        else stringHelper(visitOrder.tail, level, result + "\n" + node.toString)
      }
    }
    stringHelper(displayHelper(Vector()), -1, "Tree\n")
  }

  def diameter[A](root: Node[A]): Int = {
    /*
      LeetCode Easy
      This problem is a variation of depth-first search (DFS).
      O(N) time complexity because we must touch every node in the tree.
    */
    def diameterHelper(node: Option[Node[A]]): (Int, Int) = node match {
      case Some(x) =>
        val (leftHeight, leftDiameter) = diameterHelper(x.left)
        val (rightHeight, rightDiameter) = diameterHelper(x.right)
        val currentHeight = math.max(leftHeight, rightHeight) + 1
        val currentDiameter = math.max(leftDiameter, rightDiameter).max(leftHeight + rightHeight)
        (currentHeight, currentDiameter)
      case _ => (0, 0)
    }
    diameterHelper(Some(root))._2
  }

  def pathFromRootToNode[A](root: Node[A], target: A): Vector[A] = {
    /*
      Given the root of a Binary Tree and a `data` value representing a node,
      return the path from the root to that node in the form of a list.
      You can assume that the binary tree has nodes with unique values.
      Because this is not a binary search tree, worst time complexity is O(N)
      Depth-first search pre-order traversal, popping elements on the way back.
    */
    def searchHelper(node: Option[Node[A]], path: Vector[A]): (Vector[A], Boolean) = node match {
      case None => (path, false)
      case Some(x) if x.value.get == target => (path appended x.value.get, true)
      case Some(x) =>
        val left = searchHelper(x.left, path appended x.value.get)
        if (left._2) left else searchHelper(x.right, path appended x.value.get)
    }
    val (path, found) = searchHelper(Some(root), Vector())
    if (found) path else Vector()
  }
}

