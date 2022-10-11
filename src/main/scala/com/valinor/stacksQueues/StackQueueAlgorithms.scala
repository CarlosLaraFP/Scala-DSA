package com.valinor.stacksQueues

import scala.annotation.tailrec
import scala.util.Try
import scala.collection.immutable.{Queue => ImmutableQueue}

object StackQueueAlgorithms extends App {
  /*
    Balanced Parentheses Algorithm (LeeCode Easy)
    Verify the parentheses are balanced in a given mathematical expression.
  */
  def balancedParenthesesU(input: String): Boolean = {
    /*
      Take a string as an input and return true if its parentheses are balanced. Otherwise, return false.
      O(N) time complexity due to input string traversal.
      Order matters. Therefore, a linear stack is needed. ) => pop and ( => push
    */
    val stack = new LinearStack[Int]()
    @tailrec
    def parenthesesHelper(currentIndex: Int, lastElement: Option[Int]): Boolean =
    {
      val currentChar: Option[Char] = input.lift(currentIndex)
      if (currentChar.isEmpty && lastElement.isDefined) true
      else if (currentChar.isEmpty || lastElement.isEmpty) false
      else if (currentChar.get == '(') parenthesesHelper(currentIndex + 1, Some(stack push 1))
      else if (currentChar.get == ')') parenthesesHelper(currentIndex + 1, stack.pop())
      else parenthesesHelper(currentIndex + 1, lastElement)
    }
    if (input.isEmpty) true else parenthesesHelper(0, Some(-1))
  }

  def balancedParentheses(input: String): Boolean = {
    /*
      Take a string as an input and return true if its parentheses are balanced. Otherwise, return false.
      O(N) time complexity due to input string traversal.
      Order matters. Therefore, a linear stack is needed. ) => pop and ( => push
      Bonus: Include [ ] and { }
    */
    @tailrec
    def parenthesesHelper(currentIndex: Int, stack: List[Int]): Boolean = {
      val currentChar: Option[Char] = input.lift(currentIndex)
      if (stack.isEmpty || (currentChar.isEmpty && stack.tail.nonEmpty)) false
      else if (currentChar.isEmpty && stack.tail.isEmpty) true
      else if (currentChar.get == '(') parenthesesHelper(currentIndex + 1, 1 :: stack)
      else if (currentChar.get == ')') parenthesesHelper(currentIndex + 1, stack.tail) //stack.drop(1) also works
      else parenthesesHelper(currentIndex + 1, stack)
    }
    if (input.isEmpty) true else parenthesesHelper(0, List[Int](0))
  }

  /*
    Reverse Polish Notation (LeetCode Medium)
    Given a postfix expression as input, evaluate and return the correct final answer.
    Per LeetCode, the input is guaranteed to be valid (otherwise this takes too long).
  */
  def reversePolish(expression: Array[String]): Either[Int, String] = {
    /*
      O(N) time complexity due to full traversal
      Operator applies to 2 previous values and order matters -> Stack can only have 1 element at a time
      In a coding interview, narrow down the scope through input assumptions. Otherwise, too long.
    */
    @tailrec
    def expressionHelper(currentIndex: Int, currentTotal: Int, stack: List[Int]): Either[Int, String] = {
      expression.lift(currentIndex) match {
        case None => if (stack.isEmpty) Left(currentTotal) else Right(s"Invalid element in input array at index ${currentIndex - 1}")
        case Some("*") if stack.nonEmpty => expressionHelper(currentIndex + 1, currentTotal * stack.head, stack.tail)
        case Some("/") if stack.nonEmpty => expressionHelper(currentIndex + 1, currentTotal / stack.head, stack.tail)
        case Some("+") if stack.nonEmpty => expressionHelper(currentIndex + 1, currentTotal + stack.head, stack.tail)
        case Some("-") if stack.nonEmpty => expressionHelper(currentIndex + 1, currentTotal - stack.head, stack.tail)
        case Some(x) if Try(x.toInt).toOption.isDefined => expressionHelper(currentIndex + 1, currentTotal, x.toInt :: stack)
        case Some(x) if Try(x.toInt).toOption.isEmpty => Right(s"Invalid element in input array at index $currentIndex")
      }
    }
    if (expression.isEmpty || expression.lift(1).isEmpty || expression.lift(2).isEmpty) Right("Input array requires a minimum of 3 elements")
    else if (Try(expression.head.toInt).toOption.isEmpty) Right("Invalid element in input array at index 0")
    else expressionHelper(1, expression.head.toInt, List[Int]())
  }

  /*
    Reverse a stack.
    If your stack initially has `1, 2, 3, 4` (4 at the top and 1 at the bottom),
    after reversing the order must be `4, 3, 2, 1` (4 at the bottom and 1 at the top).
  */
  def reverseStack[A](stack: List[A]): List[A] = {
    /*
      O(N) time complexity due to traversal. O(1) space complexity due to tail recursion.
    */
    @tailrec
    def reverseHelper(original: List[A], reversed: List[A]): List[A] = {
      if (original.isEmpty) reversed else reverseHelper(original.tail, original.head :: reversed)
    }
    if (stack.isEmpty) return stack else reverseHelper(stack.tail, List(stack.head))
  }

  /*
    Minimum Bracket Reversals Algorithm (LeetCode Medium)
    Given an input string consisting of only `{` and `}`, figure out the minimum number of reversals required to make the brackets balanced.
    If the brackets cannot be balanced (odd # of elements), return `-1` to indicate that it is not possible to balance them.
  */
  def minReversalsBalanced(input: String): Int = {
    /*
      Time complexity: O(1.5 * N) due to traversal
      Since order matters, the ideal auxiliary data structure is a Stack/List (LIFO).
      1) The first Linked List only contains pairs that require flipping, either 1 or 2
      2) Total count computed separately based on conditions
    */
    @tailrec
    def minReversalsHelper(currentIndex: Int, stack: List[Char]): List[Char] = input.lift(currentIndex) match {
      case None => stack
      case Some(x) if stack.isEmpty => minReversalsHelper(currentIndex + 1, x :: stack)
      case Some(x) if x == '}' & stack.head == '{' => minReversalsHelper(currentIndex + 1, stack.tail)
      case Some(x) => minReversalsHelper(currentIndex + 1, x :: stack)
    }
    @tailrec
    def stackHelper(count: Int, list: List[Char]): Int = {
      if (list.isEmpty) count
      else if (list.head == list.tail.head) stackHelper(count + 1, list.drop(2))
      else stackHelper(count + 2, list.drop(2))
    }
    if (input.length % 2 > 0) -1 else stackHelper(0, minReversalsHelper(0, Nil))
  }

  /*
    Reversed Queue Algorithm
    Write a function that takes a queue as an input and returns a reversed version of it.
  */
  def reverseQueue[A](inputQueue: ImmutableQueue[A]): ImmutableQueue[A] = {
    /*
      Time complexity: O(N)
      Scala Queue has aC head & tail access with O(1) append
    */
    @tailrec
    def reverseHelper(newQueue: ImmutableQueue[A], queue: ImmutableQueue[A]): ImmutableQueue[A] = {
      if (queue.isEmpty) newQueue else reverseHelper(newQueue :+ queue.last, queue.dropRight(1))
    }
    reverseHelper(ImmutableQueue[A](), inputQueue)
  }
}