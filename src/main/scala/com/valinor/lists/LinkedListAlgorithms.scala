package com.valinor.lists

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object LinkedListAlgorithms extends App {

  /*
    Plus One Algorithm (LeetCode Easy)
    You are given a non-negative number in the form of list elements. For example, the number 123
    would be provided as [1, 2, 3]. Add one to the number and return the output in the form of a new list.
  */
  def addOne(sequence: IndexedSeq[Int]): IndexedSeq[Int] = {
    /*
      We assume each element in the input is a number between 0 and 9.
      Time complexity: O(1 * number of sequential 9 digits)
    */
    @tailrec
    def addOneHelper(position: Int, seq: IndexedSeq[Int]): IndexedSeq[Int] = {
      if (position < 0) 1 +: seq
      else if (seq(position) < 9) seq.updated(position, seq(position) + 1)
      else addOneHelper(position - 1, seq.updated(position, 0))
    }
    if (sequence.isEmpty) IndexedSeq[Int](1) else addOneHelper(sequence.size - 1, sequence)
  }

  /*
    Duplicate Number Algorithm (LeetCode Easy)
    You have been given an array of length n. The array contains integers from 0 to n - 2. Each number in
    the array is present exactly once except for one number which is present twice. Return this number.
  */
  def findDuplicateNumber(sequence: scala.collection.immutable.Seq[Int]): Int = {
    /*
      We know the input array must have 1 number present twice. Find it and return.
      Time complexity is O(N) and space complexity is O(1). [Linked] List is the most efficient data structure for this algorithm.
      Recursively remove the sequence element and add it to a separate Set (adding elements and lookups are eC for immutable Sets)
    */
    @tailrec
    def findDuplicateHelper(seq: scala.collection.immutable.Seq[Int], auxSet: Set[Int]): Int = {
      val currentElement = seq.head
      if (auxSet.contains(currentElement)) currentElement else findDuplicateHelper(seq.tail, auxSet + currentElement)
    }
    findDuplicateHelper(sequence, Set[Int]())
  }

  def maxSumSubArrayLC(array: Array[Int]): Int = {
    /*
      O(N) time complexity, O(1) space complexity
    */
    @tailrec
    def maxSubArrayHelper(index: Int, currSum: Int, maxSum: Int): Int = {
      if (index == array.length) maxSum
      else {
        val currentSum = currSum + array(index)
        if (currentSum > maxSum && currentSum >= 0) maxSubArrayHelper(index + 1, currentSum, currentSum)
        else if (currentSum > maxSum && currentSum < 0) maxSubArrayHelper(index + 1, 0, currentSum)
        else if (currentSum <= maxSum && currentSum < 0) maxSubArrayHelper(index + 1, 0, maxSum)
        else maxSubArrayHelper(index + 1, currentSum, maxSum)
      }
    }
    if (array.isEmpty) 0 else maxSubArrayHelper(0, 0, -10000)
  }
  /*
      Max Sum Sub Array Algorithm (LeetCode Medium)
      You have been given an array containing numbers.
      Find and return the largest sum in a contiguous sub array within the input array.
    */
  def maxSumSubArray(sequence: scala.collection.immutable.Seq[Int]): Int = {
    /*
      The entire input array must be traversed: O(N)
      List is the most efficient data structure for this algorithm, followed by Vector.
    */
    @tailrec
    def subArrayHelper(seq: scala.collection.immutable.Seq[Int], currentSum: Int, maxSum: Int): Int = {
      val currSum = currentSum + seq.headOption.getOrElse(0)
      if (seq.isEmpty) maxSum // stopping condition
      else if (currSum > maxSum && currSum >= 0) subArrayHelper(seq.tail, currSum, currSum) // regular condition
      else if (currSum > maxSum && currSum < 0) subArrayHelper(seq.tail, 0, currSum) // found negative condition
      else if (currSum <= maxSum && currSum < 0) subArrayHelper(seq.tail, 0, maxSum) // erasure condition
      else subArrayHelper(seq.tail, currSum, maxSum) // not found new max yet, continue accumulation
    }
    if (sequence.isEmpty) 0 else subArrayHelper(sequence, 0, -100000)
  }

  /*
    Pascal's Triangle Algorithm (LeetCode Easy)
    Find and return the nth row of Pascal's triangle in the form of a list. n is 0-based.
  */
  def nthRowPascal(targetIndex: Int): List[Int] = {
    /*
      Time complexity:
      Space complexity: O(1)
      Generate the next List from the previous List. We always start at 1. Prepend, head, and tail are O(1).
      Row creation is invariant whether append or prepend (symmetry).
      previousRow can be modified recursively.
    */
    @tailrec
    def pascalHelper(rowIndex: Int, previousRow: List[Int], currentRow: List[Int]): List[Int] = {
      if (rowIndex == targetIndex) previousRow
      else if (previousRow.tail == List[Int]()) pascalHelper(rowIndex + 1, 1 :: currentRow, List(1))
      else pascalHelper(rowIndex, previousRow.tail, previousRow.head + previousRow.tail.head :: currentRow)
    }
    if (targetIndex < 0) List[Int]() else pascalHelper(0, List(1), List(1))
  }

  /*
    Odd-Even Linked List (LeetCode Medium)
    Given a linked list of integers, arrange the elements such that all even nodes are placed
    after odd nodes. Maintain relative order of elements.
    Do not create any new nodes (mutability enforced?) and stick to singly linked list.
  */
  def oddEvenNodes[A](inputList: List[A]): List[A] = {
    /*
      Pure function despite internal mutability.
      Space complexity: O(1)
      Time complexity: O(N * 1.5)
    */
    @tailrec
    def oddEvenHelper(list: List[A], currentIndex: Int, oddList: ListBuffer[A], evenList: ListBuffer[A]): List[A] = {
      if (list.isEmpty) (oddList ++ evenList).toList
      else if (currentIndex % 2 == 0) oddEvenHelper(list.tail, currentIndex + 1, oddList, evenList += list.head)
      else oddEvenHelper(list.tail, currentIndex + 1, oddList += list.head, evenList)
    }
    if (inputList.isEmpty) inputList else oddEvenHelper(inputList, 1, ListBuffer[A](), ListBuffer[A]())
  }

  /*
    Skip i Delete j Algorithm (LeetCode Easy)
    You are given the head of a linked list and two integers, i and j. You have to retain the first i nodes
    and then delete the next j nodes. Continue doing so until the end of the linked list.
  */
  def skipIDeleteJ(head: List[Int], i: Int, j: Int): List[Int] = {
    /*
      Pure function with time complexity: O(N)
    */
    val newList = ListBuffer[Int]()
    @tailrec
    def deleteHelper(list: List[Int], currentIndex: Int): List[Int] = {
      if (list.isEmpty) newList.toList
      else if (currentIndex <= i)
      {
        newList += list.head
        deleteHelper(list.tail, currentIndex + 1)
      }
      else if (currentIndex <= i + j) deleteHelper(list.tail, currentIndex + 1)
      else deleteHelper(list, 1)
    }
    if (head.isEmpty) head else deleteHelper(head, 1)
  }

  /*
    Swap Nodes Algorithm (LeetCode Medium)
    Given a linked list, swap the two nodes present at position i and j, assuming 0 <= i <= j.
    The positions are based on 0-indexing. You have to swap the nodes and not just the values (?)
  */
  def swapNodes(inputList: List[Int], i: Int, j: Int): List[Int] = {
    /*
      Time complexity: O(2*N) WCS
      We use auxiliary ListBuffers due to O(1) append time complexity.
      Mutability is an option if needed for performance reasons.
    */
    val listA = ListBuffer[Int]()
    val listB = ListBuffer[Int]()
    @tailrec
    def swapNodesHelper(list: List[Int], currentIndex: Int, node: Int): List[Int] = {
      if (list.isEmpty) (listA ++ listB).toList
      else if (currentIndex == i) swapNodesHelper(list.tail, currentIndex + 1, list.head)
      else if (currentIndex < i)
      {
        listA += list.head
        swapNodesHelper(list.tail, currentIndex + 1, node)
      }
      else if (currentIndex == j || (currentIndex < j && list.tail.isEmpty)) {
        listB += node
        listA += list.head
        swapNodesHelper(list.tail, currentIndex + 1, node)
      }
      else // < j or > j
      {
        listB += list.head
        swapNodesHelper(list.tail, currentIndex + 1, node)
      }
    }
    if (inputList.isEmpty || i < 0 || i == j) inputList
    else swapNodesHelper(inputList, 0, -1)
  }
}