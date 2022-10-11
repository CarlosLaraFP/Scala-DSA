package com.valinor.recursion

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object RecursionAlgorithms extends App {
  /*
    Factorial Algorithm
  */
  def factorial(x: Int): Option[Int] = {
    /*
      O(N) time complexity
    */
    @tailrec
    def factorialHelper(currentNumber: Int, currentValue: Int): Option[Int] = {
      if (currentNumber == 0) Some(currentValue) else factorialHelper(currentNumber - 1, currentValue * currentNumber)
    }
    if (x < 0) None else factorialHelper(x, 1)
  }

  /*
    Reverse String Algorithm
  */
  def reverseString(input: String): String = {
    /*
      O(N) time complexity due to traversal
    */
    @tailrec
    def reverseHelper(currentIndex: Int, reversedString: String): String = input.lift(currentIndex) match {
      case None => reversedString
      case Some(x) => reverseHelper(currentIndex + 1, x + reversedString)
    }
    if (input.isEmpty) input else reverseHelper(0, "")
  }

  /*
    Check Palindrome Algorithm
    A palindrome is a word that is the same word when read forwards and backwards.
    For example:
    *  "madam" is a palindrome
    * "abba" is a palindrome
    *  "cat" is not
    *  "a" is a trivial case of a palindrome
    Write a function that takes a string as input and checks whether that string is a palindrome.
  */
  def checkPalindrome(input: String): Boolean = {
    /*
      Time complexity: O(N/2) by comparing first and last characters (invariance under exchange)
    */
    @tailrec
    def palindromeHelper(first: Int, last: Int): Boolean = {
      if (first >= last) true
      else if (input(first) != input(last)) false
      else palindromeHelper(first + 1, last - 1)
    }
    palindromeHelper(0, input.length - 1)
  }

  /*
    Add One Algorithm
    Problem Statement
    You are given a non-negative number in the form of list elements. For example, the number `123` would be provided as `arr = [1, 2, 3]`.
    Add one to the number and return the output in the form of a new list.
  */
  def addOneRec(sequence: IndexedSeq[Int]): IndexedSeq[Int] = {
    /*
      We assume each element in the input is a number between 0 and 9.
      Time complexity: O(N)
    */
    @tailrec
    def addOneHelper(position: Int, seq: IndexedSeq[Int]): IndexedSeq[Int] = {
      if (position < 0) 1 +: seq
      else if (seq(position) < 9) seq.updated(position, seq(position) + 1)
      else addOneHelper(position - 1, seq.updated(position, 0))
    }

    if (sequence.isEmpty) IndexedSeq[Int](1) else addOneHelper(sequence.size - 1, sequence)
  }

  def findPermutationsM(items: Array[Int]): ArrayBuffer[ArrayBuffer[Int]] = {
    /*
      Time complexity: N! + (N-1)! + (N-2)! + ...
      Start with the last element, add it to a list, then progressively add the previous element in available indices.
      Second method that returns the permutation appended
    */
    var perms = ArrayBuffer(ArrayBuffer[Int]())
    @tailrec
    def permutationsHelper(index: Int, iteration: Int, outerIndex: Int, innerIndex: Int, arrays: ArrayBuffer[ArrayBuffer[Int]]): ArrayBuffer[ArrayBuffer[Int]] = {
      if (index < 0) perms
      else if (outerIndex == perms.size)
      {
        perms = arrays
        permutationsHelper(index - 1, iteration + 1, 0, 0, ArrayBuffer[ArrayBuffer[Int]]())
      }
      else
      {
        val copy = perms(outerIndex).map(identity)
        copy.insert(innerIndex, items(index))
        arrays += copy
        if (arrays.size % iteration == 0) permutationsHelper(index, iteration, outerIndex + 1, 0, arrays)
        else permutationsHelper(index, iteration, outerIndex, innerIndex + 1, arrays)
      }
    }
    if (items.isEmpty) ArrayBuffer[ArrayBuffer[Int]]()
    else permutationsHelper(items.length - 1, 1, 0, 0, ArrayBuffer[ArrayBuffer[Int]]())
  }
  /*
     List Permutations Algorithm (LeetCode Medium)
     Given an array of items, the goal is to find all of the permutations of that array.
     For example, given an array: [0, 1, 2]
     Permutations: [[0, 1, 2], [0, 2, 1], [1, 0, 2], [1, 2, 0], [2, 0, 1], [2, 1, 0]]
   */
  def findPermutationsI(items: Array[Int]): List[List[Int]] = {
    /*
      Time complexity: N! + (N-1)! + (N-2)! + ...
      Start with the last element, add it to a list, then progressively add the previous element in available indices.
      Second method that returns the permutation appended
    */
    @tailrec
    def permutationsHelper(index: Int, iteration: Int, insertions: Int, innerIndex: Int, current: List[List[Int]], perms: List[List[Int]]): List[List[Int]] = {
      if (index < 0) perms
      else if (perms.isEmpty) permutationsHelper(index - 1, iteration + 1, 0, 0, List[List[Int]](), current)
      else if (insertions == iteration) permutationsHelper(index, iteration, 0, 0, current, perms.tail)
      else {
        val (front, back) = perms.head.splitAt(innerIndex)
        val permutation = front ++ List(items(index)) ++ back
        permutationsHelper(index, iteration, insertions + 1, innerIndex + 1, permutation :: current, perms)
      }
    }
    if (items.isEmpty) List[List[Int]]()
    else permutationsHelper(items.length - 1, 1, 0, 0, List[List[Int]](), List(List[Int]()))
  }

  /*
    String Permutations Algorithm (LeetCode Medium)
    Given an input string, return all permutations of the string in an array.
    **Example 1:**
    * `string = 'ab'`
    * `output = ['ab', 'ba']`

    **Example 2:**
    * `string = 'abc'`
    * `output = ['abc', 'bac', 'bca', 'acb', 'cab', 'cba']`
  */
  def stringPermutations(input: String): List[String] = {
    /*
      ~O(N!) time complexity
      val left = perms.head.slice(0, innerIndex)
      val right = perms.head.substring(innerIndex)
    */
    @tailrec
    def permutationHelper(index: Int, iteration: Int, innerIndex: Int, current: List[String], perms: List[String]): List[String] = {
      if (index < 0) perms
      else if (perms.isEmpty) permutationHelper(index - 1, iteration + 1, 0, List[String](), current)
      else if (innerIndex == iteration) permutationHelper(index, iteration, 0, current, perms.tail)
      else {
        val (left, right) = perms.head.splitAt(innerIndex)
        val permutation = left + input(index) + right
        permutationHelper(index, iteration, innerIndex + 1, permutation :: current, perms)
      }
    }
    if (input.isEmpty) List(input) else permutationHelper(input.length - 1, 1, 0, List[String](), List(""))
  }

  /*
    Keypad Combinations Algorithm
    Given an integer `num`, find out all the possible strings that can be made using digits of input `num`.
    Return these strings in a list. The order of strings in the list does not matter. However, the order of letters in a particular string matters.
  */
  def keypadCombinations(num: Int): List[String] = {
    /*
      Time complexity: O(4^N)
      Stopping condition, digit transition condition, inner transition condition, element processing
    */
    val number = num.toString

    def getCharacters(n: Int): String = n match {
      case 2 => "abc"
      case 3 => "def"
      case 4 => "ghi"
      case 5 => "jkl"
      case 6 => "mno"
      case 7 => "pqrs"
      case 8 => "tuv"
      case 9 => "wxyz"
      case _ => ""
    }

    @tailrec
    def keypadHelper(index: Int, iteration: Int, current: List[String], combs: List[String]): List[String] = {
      if (index == number.length) combs
      else if (combs.isEmpty) keypadHelper(index + 1, 0, List[String](), current)
      else if (iteration == getCharacters(number(index).asDigit).length) keypadHelper(index, 0, current, combs.tail)
      else {
        val char = getCharacters(number(index).asDigit)(iteration)
        keypadHelper(index, iteration + 1, (combs.head + char) :: current, combs)
      }
    }
    if (num < 0) List("") else keypadHelper(0, 0, List[String](), List[String](""))
  }

  /*
    Deep Reverse Algorithm
    Given an input list, return a new list that is the deep reverse of the input list.
    This means it reverses all the elements in the list, and if any of those elements are lists themselves,
    reverses all the elements in the inner list, all the way down.

    Input: [1, 2, [3, 4, 5], 4, 5]
    Output: [5, 4, [5, 4, 3], 2, 1]

    [1, [2, 3, [4, [5, 6]]]]
    [[[[6, 5], 4], 3, 2], 1]
  */
  def deepReverse(list: List[_]): List[_] = {
    /*
      Time complexity: O(N)
      Space complexity: locally linear, globally constant
      Amazingly, sometimes you must let go of @tailrec to solve a problem easily (such as maintaining nesting).
      This was actually the main problem I faced initially: How to keep track of arbitrary nested levels automatically.
    */
    //@tailrec
    def reverseHelper(result: List[_], input: List[_]): List[_] = {
      if (input.isEmpty) result
      else if (!input.head.isInstanceOf[List[_]]) reverseHelper(input.head :: result, input.tail)
      else {
        val reversedElement = reverseHelper(List(), input.head.asInstanceOf[List[_]])
        reverseHelper(reversedElement :: result, input.tail)
      }
    }
    reverseHelper(List(), list)
  }

  /*
    Binary Search Algorithm
    Always given a sorted indexed sequence
  */
  def binarySearch(array: IndexedSeq[Int], target: Int): Int = {
    /*
      Time complexity: O(log(N))
    */
    @tailrec
    def searchHelper(left: Int, right: Int): Int = array.lift((left + right) / 2) match {
      case _ if left > right => -1
      case Some(x) if x > target => searchHelper(left, ((left + right) / 2) - 1)
      case Some(x) if x < target => searchHelper(((left + right) / 2) + 1, right)
      case _ => (left + right) / 2
    }
    searchHelper(0, array.size - 1)
  }

  /*
    Tower of Hanoi Algorithm
  */
  def towerHanoi(disks: Int): Vector[String] = {
    /*
      Time complexity: On average, O(N), picking up approximately the same constant as N increases.
      Three rods, each a Stack (LIFO). Stopping condition + 6 cases, each with a transition condition.
      The next step is determined by the previous step, regardless of # of disks.
    */
    @tailrec
    def sourceBuilder(source: List[Int], disk: Int): List[Int] =
      if (disk == 0) source else sourceBuilder(disk :: source, disk - 1)

    @tailrec
    def rodsHelper(first: List[Int], mid: List[Int], last: List[Int], steps: Vector[String]): Vector[String] = steps.lastOption match {
      case _ if first.isEmpty & mid.isEmpty => steps
      case None | Some("DA") | Some("AD") => rodsHelper(first.tail, mid, first.head :: last, steps appended "SD")
      case Some("AS") | Some("DS") => rodsHelper(first, mid.tail, mid.head :: last, steps appended "AD")
      case Some("SD") if mid.isEmpty => rodsHelper(first.tail, first.head :: mid, last, steps appended "SA")
      case Some("SD") if mid.nonEmpty => rodsHelper(mid.head :: first, mid.tail, last, steps appended "AS")
      case Some("SA") if first.isEmpty => rodsHelper(last.head :: first, mid, last.tail, steps appended "DS")
      case Some("SA") if first.nonEmpty => rodsHelper(first, last.head :: mid, last.tail, steps appended "DA")
    }
    if (disks < 1) Vector("1 or more disks required") else rodsHelper(sourceBuilder(List(), disks), List(), List(), Vector[String]())
  }

  /*
    Return Codes Algorithm
    In an encryption system where ASCII lower case letters represent numbers in the pattern `a=1, b=2, c=3...` and so on,
    find out all the codes that are possible for a given input number.

    Return the codes in a list. The order of codes in the list is not important.
  */
  def returnCodes(number: Int): List[String] = {
    /*
      Time complexity:
      123 == List("abc", "aw", "lc")
      Can this be tail recursive? Can we discard previous iteration results?
    */
    def keys(num: Int): String = num match {
      case 1 => "a"
      case 2 => "b"
      case 3 => "c"
      case 4 => "d"
      case 5 => "e"
      case 6 => "f"
      case 7 => "g"
      case 8 => "h"
      case 9 => "i"
      case 10 => "j"
      case 11 => "k"
      case 12 => "l"
      case 13 => "m"
      case 14 => "n"
      case 15 => "o"
      case 16 => "p"
      case 17 => "q"
      case 18 => "r"
      case 19 => "s"
      case 20 => "t"
      case 21 => "u"
      case 22 => "v"
      case 23 => "w"
      case 24 => "x"
      case 25 => "y"
      case 26 => "z"
      case _ => ""
    }

    if (number == 0) List("")
    else if (number % 100 <= 26 && number > 9) //  2nd condition avoids duplicate computations
    {
      val list = returnCodes(number / 100).map(_ + keys(number % 100))
      list ++ returnCodes(number / 10).map(_ + keys(number % 10))
    }
    else returnCodes(number / 10).map(_ + keys(number % 10))
  }

  /*
    Split Array Largest Sum (LeetCode Hard)

    Given an array nums which consists of non-negative integers and an integer m,
    you can split the array into m non-empty continuous subarrays.

    Write an algorithm to minimize the largest sum among these m subarrays.
  */

  def findSubsetsH(array: IndexedSeq[Int]): IndexedSeq[IndexedSeq[Int]] = {
    /*
      Time complexity: Iterations double with every digit added
      Constant doubling rate == exponential
      Technically, the last output is all we need for the next stage.
      The approach here is in fact Head Recursion.
    */
    //@tailrec
    def subsetHelper(index: Int): IndexedSeq[IndexedSeq[Int]] = {
      if (index == array.size) IndexedSeq(IndexedSeq())
      else {
        val previous = subsetHelper(index + 1)
        val next = previous.map(IndexedSeq(array(index)) ++ _)
        previous ++ next
      }
    }
    subsetHelper(0)
  }
  /*
      Array Subsets Algorithm
      Given an integer array, find and return all the subsets of the array.
      The order of subsets in the output array is not important.
      However the order of elements in a particular subset should remain the same as in the input array.

      **Note**:
      - An empty set will be represented by an empty list.
      - If there are repeat integers, each occurrence must be treated as a separate entity.
    */
  def findSubsets(array: Array[Int]): List[List[Int]] = {
    /*
      Time complexity: Iterations double with every digit added
      Constant doubling rate == exponential
      Technically, the last output is all we need for the next stage => Tail Recursion (IT WORKED!!)
    */
    @tailrec
    def subsetHelper(index: Int, previous: List[List[Int]]): List[List[Int]] = array.lift(index) match {
      case None => previous
      case Some(x) => subsetHelper(index - 1, previous ::: previous.map(x :: _))
    }
    subsetHelper(array.length - 1, List(List()))
  }

  /*
    Staircase Algorithm
    Suppose there is a staircase that you can climb in either 1 step, 2 steps, or 3 steps.
    In how many possible ways can you climb the staircase if the staircase has `n` steps?
  */
  def staircase(steps: Int): Int = steps match {
    /*
      Time complexity: Larger than O(N)
      I had the right idea but making it more complicated. Still, this implementation is rigid.
    */
    case x if x <= 0 => 0
    case 1 => 1
    case 2 => 2
    case 3 => 4
    case _ => staircase(steps - 1) + staircase(steps - 2) + staircase(steps - 3)
  }

  /*
    Last Index Algorithm
    Given an array `arr` and a target element `target`, find the last index of occurrence of `target` in `arr`.
    If `target` is not present in `arr`, return `-1`.
  */
  def lastIndex(array: Array[Int], target: Int): Int = {
    /*
      Time complexity: O(N) due to full traversal
    */
    @tailrec
    def targetHelper(i: Int, index: Int): Int = array.lift(i) match {
      case None => index
      case Some(x) if x == target => targetHelper(i + 1, i)
      case _ => targetHelper(i + 1, index)
    }
    targetHelper(0, -1)
  }

  def staircaseQueue(steps: Int): Int = {
    /*
      Time complexity: O(N) + a constant proportional to the hop options (i.e.3 in this case)
      The auxiliary immutable Queue only ever has 3 elements.
    */
    @tailrec
    def stairHelper(step: Int, fifo: Queue[Int]): Int = {
      if (step == steps) fifo.sum else stairHelper(step + 1, fifo.dequeue._2 enqueue fifo.sum)
    }
    steps match {
      case x if x <= 0 => 0
      case 1 => 1
      case 2 => 2
      case 3 => 4
      case _ => stairHelper(4, Queue(1, 2, 4))
    }
  }
}

