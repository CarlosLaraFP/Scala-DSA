package com.valinor.LeetCode

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer, Queue => MutableQueue}
import scala.collection.immutable.{Queue => ImmutableQueue}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object LeetCodeAlgorithms extends App {

  def binarySearchLC(nums: Array[Int], target: Int): Int = {
    /*
      Given an array of integers nums which is sorted in ascending order, and an integer target,
      write a function to search target in nums. If target exists, then return its index. Otherwise, return -1.
      1, 2, 3, 4
    */
    @tailrec
    def searchHelper(left: Int, right: Int): Int = {
      if (left > right) -1
      else {
        val index = (left + right) / 2
        val element = nums(index)
        if (element == target) index
        else if (element > target) searchHelper(left, index - 1)
        else searchHelper(index + 1, right)
      }
    }
    searchHelper(0, nums.length - 1)
  }

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    /*
      Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
      You may assume that each input would have exactly one solution, and you may not use the same element twice.
      You can return the answer in any order.
      O(N) time complexity.
    */
    @tailrec
    def sumHelper(index: Int, map: Map[Int, Int]): Array[Int] = {
      if (index == nums.length) Array[Int]()
      else {
        val currentElement = nums(index)
        map.get(target - currentElement) match {
          case Some(x) => Array(x, index)
          case _ => sumHelper(index + 1, map + ((currentElement, index)))
        }
      }
    }
    sumHelper(0, Map())
  }

  def longestSubString(s: String): Int = {
    /*
      Given a string s, find the length of the longest substring without repeating characters.
      Time complexity: O(N)
      Space complexity: O(N) worst case
      Runtime: 660 ms, faster than 92.40% of Scala online submissions
      Memory Usage: 54.7 MB, less than 86.84% of Scala online submissions
    */
    @tailrec
    def traverseHelper(index: Int, currentLength: Int, map: Map[Char, Int], longest: Int): Int = {
      if (index == s.length) longest
      else {
        val currentValue = s(index)
        map.get(currentValue) match {
          case Some(x) if x >= index - currentLength =>
            val newLength = index - x
            traverseHelper(index + 1, newLength, map.updated(currentValue, index), scala.math.max(newLength, longest))
          case _ =>
            val newLength = currentLength + 1
            traverseHelper(index + 1, newLength, map + ((currentValue, index)), scala.math.max(newLength, longest))
        }
      }
    }
    traverseHelper(0, 0, Map(), 0)
  }

  def depthFirstSearch(root: TreeNode): List[Int] = {
    /*
      Return the total number of elements in the tree using DFS pre-order traversal.
      O(N) time complexity
    */
    def dfsHelper(node: TreeNode, elements: ListBuffer[Int]): ListBuffer[Int] = {
      if (node == null) elements
      else {
        val left = dfsHelper(node.left, elements appended node.value)
        dfsHelper(node.right, left)
      }
    }
    dfsHelper(root, ListBuffer()).toList
  }

  def breadthFirstSearch(root: TreeNode): Vector[Int] = {
    /*
    */
    val queue = MutableQueue(root)
    @tailrec
    def traversalHelper(visitOrder: Vector[Int]): Vector[Int] = {
      if (queue.isEmpty) visitOrder
      else {
        val node = queue.dequeue
        if (node.left != null) queue enqueue node.left
        if (node.right != null) queue enqueue node.right
        traversalHelper(visitOrder appended node.value)
      }
    }
    traversalHelper(Vector())
  }

  def maxDepth(root: TreeNode): Int = {
    /*
      DFS? Actually, it can be either DFS or BFS because all nodes must be visited regardless.
    */
    @tailrec
    def depthHelper(maxDepth: Int, queue: ImmutableQueue[(TreeNode, Int)]): Int = {
      if (queue.isEmpty) maxDepth
      else {
        val ((node, level), q) = queue.dequeue
        if (node.left != null && node.right != null)
          depthHelper(level, q enqueue ((node.left, level + 1)) enqueue ((node.right, level + 1)))
        else if (node.left != null && node.right == null)
          depthHelper(level, q enqueue ((node.left, level + 1)))
        else if (node.left == null && node.right != null)
          depthHelper(level, q enqueue ((node.right, level + 1)))
        else depthHelper(level, q)
      }
    }
    if (root == null) 0 else depthHelper(1, ImmutableQueue((root, 1)))
  }

  /*
    Characters
      Escape character: \
      Any character: .
      Digit: \d
      Not a digit: \D
      Word character: \w
      Not a word character: \W
      Whitespace: \s
      Not whitespace: \S
      Word boundary: \b
      Not a word boundary: \B
      Beginning of a string: ^
      End of a string: $
    Groupings
      Matches characters in brackets: [ ]
      Matches characters not in brackets: [^ ]
      Either or: |
      Capturing group: ( )
    Quantifiers
      0 or more: *
      1 or more: +
      0 or 1: ?
      An exact number of characters: { }
      Range of number of characters: {Minimum, Maximum}
  */
  def myAtoi(s: String): Int = {
    /*
      s consists of English letters (lower-case and upper-case), digits (0-9), ' ', '+', '-', and '.'
    */
    """^\s*([+-]?)0*(\d+)""".r.findFirstMatchIn(s) match {
      case None => 0
      case Some(m) =>
        (m.group(1) + m.group(2)).toLong match {
          case n if n > Int.MaxValue => Int.MaxValue
          case n if n < Int.MinValue => Int.MinValue
          case n => n.toInt
        }
    }
  }

  def integerToRoman(num: Int): String = {
    /*
     Given an integer, convert it to a roman numeral.
     Build string from left to right, subtracting original number until 0.
     Time complexity: O(1) where the size of the constant is proportional to the number of digits in the input.
    */
    def getRoman(value: Int): (Int, String) = value match {
      case x if x >= 1 && x < 4 => (1, "I")
      case 4 => (4, "IV")
      case x if x >= 5 && x < 9 => (5, "V")
      case 9 => (9, "IX")
      case x if x >= 10 && x < 40 => (10, "X")
      case x if x >= 40 && x < 50 => (40, "XL")
      case x if x >= 50 && x < 90 => (50, "L")
      case x if x >= 90 && x < 100 => (90, "XC")
      case x if x >= 100 && x < 400 => (100, "C")
      case x if x >= 400 && x < 500 => (400, "CD")
      case x if x >= 500 && x < 900 => (500, "D")
      case x if x >= 900 && x < 1000 => (900, "CM")
      case x if x >= 1000 && x < 4000 => (1000, "M")
      case _ => (-1, "")
    }
    @tailrec
    def stringHelper(value: Int, result: String): String = {
      if (value == 0) result
      else {
        val (number, numeral) = getRoman(value)
        stringHelper(value - number, result + numeral)
      }
    }
    stringHelper(num, "")
  }

  def romanToInteger(roman: String): Int = {
    /*
      Given a roman numeral, convert it to an integer.
      We start with 0 and add until we processed all roman numerals.
      If the next value is greater than the previous, subtract. Else, add.
    */
    def getInt(roman: Char): Int = roman match {
      case 'I' => 1
      case 'V' => 5
      case 'X' => 10
      case 'L' => 50
      case 'C' => 100
      case 'D' => 500
      case 'M' => 1000
      case _ => -1
    }
    @tailrec
    def romanHelper(index: Int, number: Int, previousValue: Int): Int = {
      if (index == roman.length) number
      else {
        val currentValue = getInt(roman(index))
        if (currentValue <= previousValue) romanHelper(index + 1, number + currentValue, currentValue)
        else romanHelper(index + 1, (number - previousValue) + (currentValue - previousValue), currentValue)
      }
    }
    romanHelper(0, 0, 5000)
  }

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    /*
      Given an integer array nums, return all the triplets [nums[i], nums[j], nums[k]]
      such that i != j, i != k, and j != k, and nums[i] + nums[j] + nums[k] == 0
      Notice that the solution set must not contain duplicate triplets, and each triplet is sorted in ascending order.
      Time complexity: O(2N + N^2) = O(N^2) in the limit of large N
    */
    // maintains each distinct value + how many different instances: O(N)
    val map = nums.foldLeft(Map.empty[Int, Int].withDefaultValue(0)) { (countMap, currentNumber) =>
      countMap + (currentNumber -> (countMap(currentNumber) + 1))
    }
    val uniqueValues = map.keys.toList
    for {
      a <- uniqueValues
      b <- uniqueValues
      if a <= b // ensures proper ascending order in triplet
      if a != b || map(a) > 1 // ensures only valid "duplicates" make it through
      c = -a - b // the value of c that satisfies: a + b + c = 0
      if b <= c // ensures proper ascending order in triplet
      candidate = List(a, b, c)
      if map(c) >= candidate.count(_ == c) // valid "duplicates" condition
    } yield candidate
  }

  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    /*
      Given an integer array nums of length n and an integer target, find three integers in nums such that the sum is closest to target.
      Return the sum of the three integers.
      You may assume that each input would have exactly one solution.
      2-pointer approach. If the current sum is less than the target, move the sum closer to it from the left. Else, lower the sum from the right.
      Time complexity: O(N^2)
      -5, -5, -4, 0, 0, 3, 3, 4, 5  ->  -2
    */
    val sorted = nums.sorted
    @tailrec
    def closestHelper(index: Int, low: Int, high: Int, smallestDiff: Int): Int = {
      if (index == sorted.length || smallestDiff == 0) target - smallestDiff
      else if (low >= high) closestHelper(index + 1, index + 2, sorted.length - 1, smallestDiff)
      else {
        val sum = sorted(index) + sorted(low) + sorted(high)
        val diff = if (scala.math.abs(target - sum) < scala.math.abs(smallestDiff)) target - sum else smallestDiff
        if (sum < target) closestHelper(index, low + 1, high, diff) else closestHelper(index, low, high - 1, diff)
      }
    }
    closestHelper(0, 1, sorted.length - 1, target - sorted.take(3).sum)
  }

  def fourSum(nums: Array[Int], target: Int): List[List[Int]] = {
    /*
      Given an array nums of n integers, return an array of all the unique quadruplets [nums[a], nums[b], nums[c], nums[d]] such that:
      0 <= a, b, c, d < n
      a, b, c, and d are distinct.
      nums[a] + nums[b] + nums[c] + nums[d] == target
      You may return the answer in any order.
    */
    val countsMap = nums.foldLeft(Map.empty[Int, Int].withDefaultValue(0))((map, currentNumber) =>
      map + (currentNumber -> (map(currentNumber) + 1))
    )
    val uniqueElements = countsMap.keys.toList
    for {
      a <- uniqueElements
      b <- uniqueElements
      c <- uniqueElements
      if a <= b && (a != b || countsMap(a) > 1)
      if b <= c && countsMap(c) >= List(a, b, c).count(_ == c)
      d = target - a.toLong - b.toLong - c.toLong
      if c <= d && d >= Int.MinValue && d <= Int.MaxValue
      quadruplet = List(a, b, c, d.toInt)
      if countsMap(d.toInt) >= quadruplet.count(_ == d)
    } yield quadruplet
  }

  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    /*
      Given an array of strings, group the anagrams together. You can return the answer in any order.
      An Anagram is a word or phrase formed by rearranging the letters of a different word or phrase,
      typically using all the original letters exactly once. Duplicates are valid anagrams of each other.
      1. Two strings are anagrams if and only if their sorted strings are equal. Map[sortedString, List[String]]
      2. Two strings are anagrams if and only if their character counts (respective number of occurrences of each character) are the same.
    */
    @tailrec
    def anagramHelper(index: Int, map: Map[String, List[String]]): Map[String, List[String]] = {
      if (index < 0) map
      else {
        val currentValue = strs(index)
        val currentSorted = currentValue.sorted
        map.get(currentSorted) match {
          case None => anagramHelper(index - 1, map + ((currentSorted, List(currentValue))))
          case Some(x) => anagramHelper(index - 1, map.updated(currentSorted, currentValue :: x))
        }
      }
    }
    anagramHelper(strs.length - 1, Map()).values.toList
  }

  def spiralMatrix(matrix: Array[Array[Int]]): List[Int] = {
    /*
      Given an m x n matrix, return all elements of the matrix in spiral order.
      O(1) space complexity
      We transpose the tail only at the turns + they get smaller every time = number of columns N
      Time complexity: O(M*N) due to visiting each element once + a constant
      In the limit of very large M and N, turns become rare and the main term dominates => O(M*N)
    */
    @tailrec
    def spiralHelper(currentMatrix: Array[Array[Int]], spiral: List[Int]): List[Int] = currentMatrix match {
      case x if x.isEmpty => spiral
      case x if x.length == 1 => spiral ::: currentMatrix.head.toList
      case _ => spiralHelper(currentMatrix.tail.transpose.reverse, spiral ::: currentMatrix.head.toList)
    }
    spiralHelper(matrix, List())
  }

  def minimumWindowSubstring(s: String, t: String): String = {
    /*
      Given two strings s and t of lengths m and n respectively,
      return the minimum window substring of s such that every character in t (including duplicates) is included in the window.
      If there is no such substring, return the empty string "".

      1. Create Map of target character counts, with default value 0
      2. Create Map of target characters found in source, with counts found + first window boundaries (0, stopping index)
      3. Slide right index until the left character count exceeds true count, in which case slide left index; maintain minimum window
      Time complexity: O(|t|) + O(|s|)
      Space complexity: O(|t|) + O(|s|)
    */
    //val targetMap = t.toList.groupBy(identity).map { case (c, l) => (c, l.length) }
    val targetMap = t.foldLeft(Map[Char, Int]().withDefaultValue(0))((map, currentElement) => map + ((currentElement, map(currentElement) + 1)))
    @tailrec
    def minHelper(left: Int, right: Int, sourceMap: Map[Char, Int], minimumWindow: (Int, Int)): (Int, Int) = {
      val currentLeft = s(left)
      val newWindow = if (right - left < minimumWindow._2 - minimumWindow._1) (left, right) else minimumWindow
      if (sourceMap(currentLeft) > targetMap(currentLeft)) minHelper(left + 1, right, sourceMap.updated(currentLeft, sourceMap(currentLeft) - 1), newWindow)
      else if (right < s.length) minHelper(left, right + 1, sourceMap.updated(s(right), sourceMap(s(right)) + 1), newWindow)
      else newWindow
    }
    @tailrec
    def firstHelper(i: Int, sourceMap: Map[Char, Int]): (Int, Map[Char, Int]) = {
      if (!targetMap.exists { case (targetChar: Char, count: Int) => sourceMap(targetChar) < count }) (i, sourceMap)
      else if (i < s.length) firstHelper(i + 1, sourceMap.updated(s(i), sourceMap(s(i)) + 1))
      else (0, sourceMap)
    }
    val (rightIndex, sourceMap) = firstHelper(0, Map.empty[Char, Int].withDefaultValue(0))
    if (rightIndex == 0) ""
    else {
      val (left, right) = minHelper(0, rightIndex, sourceMap, (0, rightIndex))
      s.slice(left, right)
    }
  }

  def validPalindrome(s: String): Boolean = {
    /*
      A phrase is a palindrome if, after converting all uppercase letters into lowercase letters
      and removing all non-alphanumeric characters, it reads the same forward and backward.
      Alphanumeric characters include letters and numbers.
      Given a string s, return true if it is a palindrome, or false otherwise.
      Worst time complexity: O(N)
    */
    @tailrec
    def palindromeHelper(left: Int, right: Int, isPalindrome: Boolean): Boolean = {
      if (right < left) isPalindrome
      else if (!s(left).isLetterOrDigit) palindromeHelper(left + 1, right, isPalindrome)
      else if (!s(right).isLetterOrDigit) palindromeHelper(left, right - 1, isPalindrome)
      else if (s(left).toLower == s(right).toLower) palindromeHelper(left + 1, right - 1, isPalindrome)
      else false
    }
    palindromeHelper(0, s.length - 1, isPalindrome = true)
  }

  def majorityElementII(nums: Array[Int]): List[Int] = {
    /*
      Given an integer array of size n, find all elements that appear more than ⌊ n/3 ⌋ times.
      The particular integer value is irrelevant. All that matters is the number of appearances each.
      Solve this problem in O(N) time and in O(1) space.
      Apple: Focus on pragmatic solutions.
      nums.groupBy(identity).filter(_._2.length > nums.length / 3).keys.toList
    */
    nums.
      foldLeft(Array.ofDim[Int](4)) {
        case (cache, current) if current == cache(0) => cache.updated(2, cache(2) + 1)
        case (cache, current) if current == cache(1) => cache.updated(3, cache(3) + 1)
        case (cache, current) if cache(2) == 0 => cache.updated(0, current).updated(2, 1)
        case (cache, current) if cache(3) == 0 => cache.updated(1, current).updated(3, 1)
        case (cache, _) => cache.updated(2, cache(2) - 1).updated(3, cache(3) - 1)
      }.
      take(2). // Ignoring counts
      distinct. // Ignoring duplicates
      filter(n => nums.count(_ == n) > nums.length / 3).
      toList
  }

  def arrayProduct(nums: Array[Int]): Array[Int] = {
    /*
      Given an integer array nums, return an array answer such that answer[i] is equal to the product of all the elements of nums except nums[i].
      The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
      You must write an algorithm that runs in O(n) time and without using the division operation.
      Can you solve the problem in O(1) extra space complexity? (The output array does not count as extra space for space complexity analysis.)
      Time complexity: O(2N)
    */
    if (nums.length <= 1) Array[Int]()
    else {
      @tailrec
      def productHelper(index: Int, result: Array[Int], rightProduct: Int): Array[Int] = {
        if (index < 0) result else productHelper(index - 1, result.updated(index, result(index) * rightProduct), nums(index) * rightProduct)
      }
      productHelper(nums.length - 1, nums.scanLeft(1)(_ * _).dropRight(1), 1)
    }
  }

  def missingNumber(nums: Array[Int]): Int = {
    /*
      Given an array nums containing n distinct numbers in the range [0, n],
      return the only number in the range that is missing from the array.
      Gauss' Formula: sum of the first n natural numbers.
      Time complexity: O(N)
    */
    val gaussSum: Int = nums.length * (nums.length + 1) / 2
    gaussSum - nums.sum
  }

  def firstUniqueChar(s: String): Int = {
    /*
      Given a string s, find the first non-repeating character in it and return its index.
      If it does not exist, return -1
      This is O(2N)
    */
    val charCounts = s.foldLeft(Map.empty[Char, Int].withDefaultValue(0)) {
      case (map, char) => map.updated(char, map(char) + 1)
    }
    @tailrec
    def firstHelper(index: Int): Int = {
      if (index == s.length) -1
      else if (charCounts(s(index)) == 1) index
      else firstHelper(index + 1)
    }
    firstHelper(0)
  }


}

