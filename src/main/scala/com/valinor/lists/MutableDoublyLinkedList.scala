package com.valinor.lists

import scala.annotation.tailrec
import scala.language.postfixOps

final class Node[A](v: A) {
  /*
    Mutable, invariant Node class for Doubly Linked List.
  */
  private var _value: A = v
  def value: A = _value
  def value_= (newValue: A): Unit = {
    _value = newValue
  }

  private var _next: Option[Node[A]] = None
  def next: Option[Node[A]] = _next
  def next_= (newValue: Option[Node[A]]): Unit = {
    _next = newValue
  }

  private var _previous: Option[Node[A]] = None
  def previous: Option[Node[A]] = _previous
  def previous_= (newValue: Option[Node[A]]): Unit = {
    _previous = newValue
  }

  override def toString = s"$value"
}

sealed trait LinearSequence[A] {
  /*
    Reusable trait to build Linked List classes through mix-in composition.
    If mutable covariant: protected def prepend[B >: A](node: Node[B]): Unit
  */
  protected def prepend(value: A): Unit
  protected def append(value: A): Unit
  protected def search(value: A): Option[Node[A]]
  protected def remove(value: A): Unit
  protected def pop(): Option[A]
  protected def insert(value: A, position: Int): Unit
  protected def size(): Int
  protected def getElements: IndexedSeq[A]
  protected def reverse(): LinearSequence[A]
  protected def isCircular: Boolean
}

final class DoublyLinkedList[A] extends LinearSequence[A] {
  /*
    Mutable, invariant class.
    Contains head and tail properties + methods. Head and tail can only be set privately through methods.
  */
  private var _head: Option[Node[A]] = None
  def head: Option[Node[A]] = _head

  private var _tail: Option[Node[A]] = head
  def tail: Option[Node[A]] = _tail

  override def size(): Int = getElements.size

  @tailrec
  private def elementHelper(elements: Vector[A], currentNode: Option[Node[A]]): Vector[A] = {
    /*
      O(N) due to traversal
      Immutable covariant Vector[A] extends covariant IndexedSeq[A]
    */
    if (currentNode.isEmpty) elements else elementHelper(elements :+ currentNode.get.value, currentNode.get.next)
  }
  override def getElements: Vector[A] = elementHelper(Vector[A](), _head)

  override def prepend(value: A): Unit = {
    /*
      Prepends Node object in O(1)
      Mutable container methods can return unit due to private mutations.
      Immutable container methods (i.e. prepended) return a new instance with the added mutations.
    */
    val newNode = Some(new Node[A](value))

    if (_head.isEmpty)
    {
      _head = newNode
      _tail = _head
    }
    else
    {
      val currentHead = _head
      currentHead.get.previous = newNode
      newNode.get.next = currentHead
      _head = newNode
    }
    //_elements prepend newNode.value
  }

  override def append(value: A): Unit = {
    /*
      Appends Node object in O(1) if we maintain a reference to tail (Yes)
      Appends Node object in O(N) if we do not maintain a reference to tail (why would we do this? lower memory footprint?)
    */
    val newNode = Some(new Node[A](value))

    if (_head.isEmpty)
    {
      _head = newNode
      _tail = _head
    }
    else
    {
      val currentTail = _tail
      currentTail.get.next = newNode
      newNode.get.previous = currentTail
      _tail = newNode
    }
    //_elements append newNode.value
  }

  @tailrec
  private def searchHelper(x: A, currentNode: Option[Node[A]]): Option[Node[A]] = {
    /*
      Scala discourages the use of vars and explicit loops.
      The use of recursion is a more functional approach to writing loops.
      Scala highly encourages the use of functional recursive loops.

      Tail recursion means O(1) space complexity
      Linear traversal means O(N) time complexity
    */
    if (currentNode.isEmpty || currentNode.get.value == x) currentNode else searchHelper(x, currentNode.get.next)
  }
  override def search(value: A): Option[Node[A]] = searchHelper(value, _head)

  override def remove(value: A): Unit = {
    /*
      Remove a node (first occurrence of value)
      O(N) time and O(1) space
    */
    val targetNode = searchHelper(value, _head)

    if (targetNode.isDefined)
    {
      val previousNode = targetNode.get.previous
      val nextNode = targetNode.get.next
      if (previousNode.isDefined) previousNode.get.next = nextNode else _head = nextNode
      if (nextNode.isDefined) nextNode.get.previous = previousNode else _tail = previousNode
    }
  }

  override def pop(): Option[A] = {
    /*
      Pop, which means to return the first node's value and delete the node from the list
      O(1) in both time and space
    */
    if (_head.isDefined)
    {
      val headValue = _head.get.value
      this.remove(headValue)
      Some(headValue)
    }
    else None
  }

  @tailrec
  private def insertHelper(value: A, targetPosition: Int, currentPosition: Int, currentNode: Option[Node[A]]): Unit = {
    /*
      Insert data at some position in the list
      O(N) time complexity
      Insertion at position adds to the left of the node if found
    */
    if (currentNode.isEmpty) this.append(value)
    else if (targetPosition == currentPosition)
    {
      val newNode = Some(new Node[A](value))
      val previous = currentNode.get.previous
      newNode.get.next = currentNode
      newNode.get.previous = previous
      currentNode.get.previous = newNode
      if (previous.isEmpty) _head = newNode
    }
    else insertHelper(value, targetPosition, currentPosition + 1, currentNode.get.next)
  }
  override def insert(value: A, position: Int): Unit = insertHelper(value=value, position, 0, _head)

  @tailrec
  private def reverseHelper(linkedList: DoublyLinkedList[A], currentNode: Option[Node[A]]): DoublyLinkedList[A] = {
    /*
      O(N) time complexity, using recursion starting with the head Node
      This would work particularly well with immutable.List appended method.
    */
    if (currentNode.isEmpty) linkedList
    else
    {
      linkedList prepend currentNode.get.value
      reverseHelper(linkedList, currentNode.get.next)
    }
  }
  override def reverse(): DoublyLinkedList[A] = reverseHelper(new DoublyLinkedList[A](), _head)

  @tailrec
  private def circularHelper(slow: Option[Node[A]], fast: Option[Node[A]]): Boolean = {
    /*
      Returns true if a loop exists in the Linked List; false otherwise
      O(N) time complexity due to traversal and O(1) space complexity
      Use 2 pointers called runners moving through the list at different rates. Slow 1 node/step and Fast 2 nodes/step.
    */
    if (fast.isEmpty || fast.get.next.isEmpty) false
    else if (slow.get.next == fast.get.next.get.next) true
    else circularHelper(slow.get.next, fast.get.next.get.next)
  }
  override def isCircular: Boolean = circularHelper(_head, _head)

  def createCircularRelation(): Unit = _tail.get.next = _head
}

final class SortedLinkedList {
  /*
  */
  private var _head: Option[Node[Int]] = None
  def head: Option[Node[Int]] = _head

  @tailrec
  private def elementHelper(elements: Vector[Int], currentNode: Option[Node[Int]]): Vector[Int] = {
    /*
      O(N) due to traversal
      Immutable covariant Vector[A] extends covariant IndexedSeq[A]
    */
    if (currentNode.isEmpty) elements else elementHelper(elements :+ currentNode.get.value, currentNode.get.next)
  }
  def getElements: Vector[Int] = elementHelper(Vector[Int](), _head)

  @tailrec
  private def sortedAppend(newNode: Option[Node[Int]], currentNode: Option[Node[Int]]): Unit = {
    /*
      Add element in numeric order. This is essentially prepend + append + insert in a single method.
      At append time, the linked list is guaranteed to already be ordered.
      O(N) time complexity due to traversal.
    */
    if (currentNode.isEmpty) _head = newNode
    else if (currentNode.get.value >= newNode.get.value)
    {
      newNode.get.next = currentNode
      newNode.get.previous = currentNode.get.previous
      currentNode.get.previous = newNode
      if (newNode.get.previous.isEmpty) _head = newNode else newNode.get.previous.get.next = newNode
    }
    else if (currentNode.get.next.isEmpty)
    {
      newNode.get.previous = currentNode
      currentNode.get.next = newNode
    }
    else sortedAppend(newNode, currentNode.get.next)
  }
  def append(value: Int): Unit = sortedAppend(Some(new Node(value)), _head)

  def sortSequence(array: IndexedSeq[Int]): Vector[Int] = {
    /*
      O(2N) time complexity
      1) Generate a SortedLinkedList out of array
      2) Traverse the SortedLinkedList recursively, incrementally returning a new immutable Vector with the value appended
    */
    @tailrec
    def linearAppend(array: Vector[Int], currentNode: Option[Node[Int]]): Vector[Int] = currentNode match {
      case None => array
      case _ => linearAppend(array :+ currentNode.get.value, currentNode.get.next)
    }
    val list = new SortedLinkedList()
    array.foreach(list append)
    linearAppend(Vector[Int](), list.head)
  }
}

final class NestedSortedLinkedList {
  /*
  */
  private var _head: Option[Node[SortedLinkedList]] = None
  def head: Option[Node[SortedLinkedList]] = _head

  def prepend(value: SortedLinkedList): Unit = {
    /*
    */
    val newHead = Some(new Node(value))
    if (_head.isEmpty) _head = newHead
    else
    {
      newHead.get.next = _head
      _head = newHead
    }
  }

  def flatten: SortedLinkedList = {
    /*
      If there are 2+ Nodes, use recursion to sortedAppend each element from firstList to secondList and return secondList
      I think this is O(M * N * C)
    */
    @tailrec
    def merge(currentNode: Option[Node[SortedLinkedList]], nextNode: Option[Node[SortedLinkedList]]): SortedLinkedList = {
      if (nextNode.isEmpty) currentNode.get.value
      else
      {
        currentNode.get.value.getElements.foreach(nextNode.get.value append _) // with more time, this could be just O(N)
        merge(nextNode, nextNode.get.next)
      }
    }
    if (_head.isEmpty) new SortedLinkedList()
    else merge(_head, _head.get.next)
  }
}

