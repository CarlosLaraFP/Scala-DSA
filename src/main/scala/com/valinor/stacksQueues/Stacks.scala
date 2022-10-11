package com.valinor.stacksQueues

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.annotation.tailrec

/*
  Implement the following methods:
  1. push (adds an item to the top of the stack)
  2. pop (removes an item from the top of the stack and returns the value of that item)
  3. size (returns the size of the stack)
  4. top (returns the value of the item at the top of the stack, without removing that item)
  5. isEmpty (returns true if the stack is empty and false otherwise)
*/
sealed trait Stack[A] {
  protected def numElements: Int
  protected def push(value: A): A
  protected def pop(): Option[A]
  protected def top(): Option[A]
}

final class LinearStack[A] extends Stack[A] {
  /*
    Mutable, invariant Stack class implemented with a linked list.
    LIFO stack-like access pattern.
    Note: List is the go-to immutable linear sequence.
  */
  private var _elements: List[A] = List[A]()
  def elements: List[A] = _elements

  private var _numElements: Int = 0
  override def numElements: Int = _numElements

  override def push(value: A): A = {
    /*
      Adds an item to the top of the stack.
      O(1) time complexity due to linked list prepend.
    */
    _elements = value :: _elements
    _numElements += 1
    value
  }

  override def pop(): Option[A] = {
    /*
      Removes an item from the top of the stack and returns the value of that item.
      O(1) time complexity due to constant head and tail access for immutable List.
    */
    val topElement = _elements.headOption
    if (topElement.isDefined)
    {
      _elements = _elements.tail
      _numElements -= 1
    }
    topElement
  }

  override def top(): Option[A] = _elements.headOption

  def bottom(): Option[A] = _elements.lastOption // O(N) for List
}

final class IndexedStack[A] extends Stack[A] {
  /*
    Mutable, invariant Stack class implemented with an array.
    Note: Vector is the go-to immutable indexed sequence.
  */
  private val _array: ArrayBuffer[A] = ArrayBuffer[A]()
  def array: ArrayBuffer[A] = _array

  override def numElements: Int = _array.size

  override def push(value: A): A = {
    /*
      Adds an item to the top of the stack (end of the array).
      O(1) time complexity, aC due to ArrayBuffer
    */
    _array += value
    value
  }

  override def pop(): Option[A] = {
    /*
      Removes an item from the top of the stack and returns the value of that item
      O(1) time complexity
    */
    val topElement = _array.lastOption
    _array.remove(numElements - 1)
    topElement
  }

  override def top(): Option[A] = _array.lastOption
}

/*
  Once implemented, our queue will need to have the following functionality:
  1. `enqueue`  - adds data to the back of the queue
  2. `dequeue`  - removes data from the front of the queue
  3. `front`    - returns the element at the front of the queue
  4. `size`     - returns the number of elements present in the queue
  5. `is_empty` - returns `True` if there are no elements in the queue, and `False` otherwise
  6. `_handle_full_capacity` - increases the capacity of the array, for cases in which the queue would otherwise overflow

  Also, if the queue is empty, `dequeue` and `front` operations should return `None`.
*/
sealed trait Queue[A] {
  protected def enqueue(value: A): Unit
  protected def dequeue(): Option[A]
  protected def front(): Option[A]
  protected def size(): Int
  protected def isEmpty: Boolean
}

final class LinearQueue[A] extends Queue[A] {
  private val _list = new ListBuffer[A]()

  override def size(): Int = _list.length

  override def isEmpty: Boolean = _list.isEmpty

  override def front(): Option[A] = _list.lastOption

  override def enqueue(value: A): Unit = _list prepend value

  override def dequeue(): Option[A] = {
    /*
      Since a Queue is FIFO, this method removes & returns the oldest element added
      From the ListBuffer source code, last/lastOption returns a reference to the last element in O(1)
      By extension, removing the last element should also be O(1) => confirmed in ListBuffer source code
      Finally, ListBuffer also maintains a reference to its size with O(1) access
    */
    val oldest = front()
    _list.remove(size - 1)
    oldest
  }
}

final class IndexedQueue[A] extends Queue[A] {
  private var _array = new ArrayBuffer[A]()
  override def size(): Int = _array.size
  override def isEmpty: Boolean = _array.isEmpty
  override def front(): Option[A] = _array.headOption

  /*
    This is an append when using an indexed sequence
    aC time complexity
  */
  override def enqueue(value: A): Unit = _array += value

  override def dequeue(): Option[A] = {
    /*
      Since a Queue is FIFO, this method removes & returns the oldest element appended (head)
      O(N) due to tail call on ArrayBuffer
    */
    val oldest = front()
    _array = _array.drop(1)
    oldest
  }
}

final class StackedQueue[A] extends Queue[A] {
  private var _stack = new LinearStack[A]()

  override def size(): Int = _stack.numElements

  override def isEmpty: Boolean = size == 0

  override def front(): Option[A] = _stack.bottom()

  override def enqueue(value: A): Unit = _stack.push(value)

  override def dequeue(): Option[A] = {
    /*
      O(N) time complexity to access the bottom (first added) element in a Stack
      while pushing the popped elements into a new Stack
    */
    var tempList = List[A]()
    _stack.elements.foreach(e => tempList = e :: tempList)

    val oldest = tempList.headOption

    val newStack = new LinearStack[A]()
    if (oldest.isDefined) tempList.tail.foreach(c => newStack push c)
    _stack = newStack

    oldest
  }
}

