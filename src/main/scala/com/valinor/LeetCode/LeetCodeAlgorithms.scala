package com.valinor.LeetCode

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer, Queue => MutableQueue}
import scala.collection.immutable.{Queue => ImmutableQueue}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

class Node(var _value: Int) {
  var value: Int = _value
  var neighbors: List[Node] = List()
}

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
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
    @tailrec
    def productHelper(index: Int, result: Vector[Int], rightProduct: Int): Array[Int] = {
      if (index < 0) result.toArray
      else productHelper(index - 1, result.updated(index, result(index) * rightProduct), nums(index) * rightProduct)
    }
    if (nums.length <= 1) Array[Int]()
    else {
      val intermediateResult = nums.
        foldLeft(Vector(1)){ case (array, n) => array appended array.last * n }.
        dropRight(1)

      productHelper(nums.length - 1, intermediateResult, 1)
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

  def subarraySumK(nums: Array[Int], k: Int): Int = {
    /*
      Given an array of integers nums and an integer k, return the total number of subarrays whose sum equals to k.
      A subarray is a contiguous non-empty sequence of elements within an array.
      First, what is the time complexity of this problem? O(N) with a Map (space O(N))
      Store the cumulative sum at each index in a Map[cumulative sum, total occurrences of cumulative sum]
      If the difference came before, we just crossed a valid subarray
    */
    @tailrec
    def sumHelper(index: Int, map: Map[Int, Int], cumulativeSum: Int, count: Int): Int = {
      if (index == nums.length) count
      else {
        val sum = cumulativeSum + nums(index)
        val updatedCount = if (map.contains(sum - k)) count + map(sum - k) else count
        sumHelper(index + 1, map.updated(sum, map(sum) + 1), sum, updatedCount)
      }
    }
    sumHelper(0, Map(0 -> 1).withDefaultValue(0), 0, 0)
  }

  def squaresSortedArray(nums: Array[Int]): Array[Int] = {
    /*
      Given an integer array nums sorted in non-decreasing order,
      return an array of the squares of each number sorted in non-decreasing order.
      Follow up: Squaring each element and sorting the new array is very trivial, could you find an O(n) solution using a different approach?

      Array.ofDim[Int](nums.length)
    */
    @tailrec
    def squaresHelper(i: Int, j: Int, squares: List[Int]): Array[Int] = {
      if (i > j) squares.toArray
      else if (i == j) squares.prepended(nums(i) * nums(j)).toArray
      else {
        val left = nums(i) * nums(i)
        val right = nums(j) * nums(j)
        if (right > left) squaresHelper(i, j - 1, right :: squares)
        else if (left > right) squaresHelper(i + 1, j, left :: squares)
        else squaresHelper(i + 1, j - 1, left :: right :: squares)
      }
    }
    squaresHelper(0, nums.length - 1, List())
  }

  def validParentheses(s: String): Boolean = {
    /*
      Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.
      An input string is valid if:
      Open brackets must be closed by the same type of brackets.
      Open brackets must be closed in the correct order.
      Every close bracket has a corresponding open bracket of the same type.
      Push: (, {, [
      Pop: ), }, ]
      Time complexity: O(N)
    */
    val stack = s.foldLeft(List[Char]()) {
      case (stack, char) if char == '(' || char == '{' || char == '[' => char :: stack
      case (stack, char) if char == ')' && stack.headOption.contains('(') => stack.tail
      case (stack, char) if char == '}' && stack.headOption.contains('{') => stack.tail
      case (stack, char) if char == ']' && stack.headOption.contains('[') => stack.tail
      case (stack, char) => char :: stack
    }
    if (stack.isEmpty) true else false
  }

  def trappingRainWater(height: Array[Int]): Int = {
    /*
      Given n non-negative integers representing an elevation map where the width of each bar is 1,
      compute how much water it can trap after raining.
      For each element in the array, we find the maximum level of water it can trap after the rain,
      which is equal to the minimum of maximum height of bars on both the sides minus its own height.
    */
    import scala.math.max
    @tailrec
    def countHelper(left: Int, right: Int, currentHighest: Int, count: Int): Int = {
      if (left >= right) count
      else if (height(left) <= height(right))
        countHelper(left + 1, right, max(currentHighest, height(left)), count + max(0, currentHighest - height(left)))
      else
        countHelper(left, right - 1, max(currentHighest, height(right)), count + max(0, currentHighest - height(right)))
    }
    countHelper(0, height.length - 1, 0, 0)
  }

  def sparseMatrixMultiplicationN(mat1: Array[Array[Int]], mat2: Array[Array[Int]]): Array[Array[Int]] = {
    /*
      Given two sparse matrices mat1 of size m x k and mat2 of size k x n, return the result of mat1 x mat2.
      You may assume that multiplication is always possible.
      Time complexity analysis: O(m * k * n) + the k*n transpose
    */
    val matB = mat2.transpose
    @tailrec
    def matrixHelper(i: Int, j: Int, currentRow: Array[Int], result: Array[Array[Int]]): Array[Array[Int]] = {
      if (i == mat1.length) result
      else if (j == matB.length) matrixHelper(i + 1, 0, Array(), result :+ currentRow)
      else {
        val element = mat1(i).zip(matB(j)).map(r => r._1 * r._2).sum
        matrixHelper(i, j + 1, currentRow :+ element, result)
      }
    }
    matrixHelper(0, 0, Array(), Array())
  }

  def sparseMatrixMultiplication(mat1: Array[Array[Int]], mat2: Array[Array[Int]]): Array[Array[Int]] = {
    /*
      Given two sparse matrices mat1 of size m x k and mat2 of size k x n, return the result of mat1 x mat2.
      You may assume that multiplication is always possible.
      Time complexity analysis: O(m * k * n)
      Space complexity: O(m * k * n)
    */
    @tailrec
    def compressMatrix(i: Int, j: Int, matrix: Array[Array[Int]], compressed: Array[Array[(Int, Int)]]): Array[Array[(Int, Int)]] = {
      /*
        Inputs matrix and returns compressedMatrix with only non-zero elements.
        To build compressedMatrix, we iterate over each element of matrix and
        if the element is non-zero push the (value, col) pair in the respective row of compressedMatrix.
      */
      if (i == matrix.length) compressed
      else if (j == matrix.head.length) compressMatrix(i + 1, 0, matrix, compressed)
      else if (matrix(i)(j) == 0) compressMatrix(i, j + 1, matrix, compressed)
      else compressMatrix(i, j + 1, matrix, compressed.updated(i, compressed(i) :+ (matrix(i)(j), j)))
    }
    val compressedA = compressMatrix(0, 0, mat1, Array.fill(mat1.length) { Array() })
    val compressedB = compressMatrix(0, 0, mat2, Array.fill(mat2.length) { Array() })
    val result = Array.fill(mat1.length) { Array.ofDim[Int](mat2.head.length) }
    for {
      m <- mat1.indices
      (elementA, k) <- compressedA(m)
      (elementB, n) <- compressedB(k)
    } result(m)(n) += elementA * elementB
    result
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    /*
      You are given two non-empty linked lists representing two non-negative integers.
      The digits are stored in reverse order, and each of their nodes contains a single digit.
      Add the two numbers and return the sum as a linked list.
      You may assume the two numbers do not contain any leading zero, except the number 0 itself.
      Time complexity: O(N)
    */
    @tailrec
    def getReversedNumber(currentNode: ListNode, number: String): BigInt = {
      if (currentNode == null) scala.math.BigInt.apply(number)
      else getReversedNumber(currentNode.next, currentNode.x.toString + number)
    }
    val sum = (getReversedNumber(l1, "") + getReversedNumber(l2, "")).toString
    val root = new ListNode(sum.last.asDigit)
    @tailrec
    def buildLinkedList(currentNode: ListNode, index: Int): Unit = {
      if (index >= 0)  {
        currentNode.next = new ListNode(sum(index).asDigit)
        buildLinkedList(currentNode.next, index - 1)
      }
    }
    buildLinkedList(root, sum.length - 2)
    root
  }

  def mergeSortedLists(list1: ListNode, list2: ListNode): ListNode = {
    /*
      You are given the heads of two sorted linked lists list1 and list2.
      Merge the two lists in a one sorted list. The list should be made by splicing together the nodes of the first two lists.
      Return the head of the merged linked list.
      This should be done with immutable Lists or a properly structured custom DS.
      2-pointer approach
      O(N) time complexity
    */
    val mergedList = new ListNode()
    @tailrec
    def sortHelper(current1: ListNode, current2: ListNode, currentFinal: ListNode): ListNode = (current1, current2) match {
      case (null, null) => mergedList
      case (node1, null) =>
        currentFinal.next = node1
        mergedList
      case (null, node2) =>
        currentFinal.next = node2
        mergedList
      case (node1, node2) if node1.x >= node2.x =>
        currentFinal.next = new ListNode(node2.x)
        sortHelper(node1, node2.next, currentFinal.next)
      case (node1, node2) if node1.x < node2.x =>
        currentFinal.next = new ListNode(node1.x)
        sortHelper(node1.next, node2, currentFinal.next)
    }
    if (list1 == null && list2 == null) null else if (list1 == null) list2 else if (list2 == null) list1
    else if (list1.x <= list2.x) {
      mergedList.x = list1.x
      sortHelper(list1.next, list2, mergedList)
    }
    else {
      mergedList.x = list2.x
      sortHelper(list1, list2.next, mergedList)
    }
  }

  def reverseLinkedList(head: ListNode): ListNode = {
    /*
      Given the head of a singly linked list, reverse the list, and return the reversed list.
      O(N) time complexity
    */
    @tailrec
    def reverseHelper(currentNode: ListNode, reversed: ListNode = null): ListNode = {
      if (currentNode == null) reversed else reverseHelper(currentNode.next, new ListNode(currentNode.x, reversed))
    }
    reverseHelper(head)
  }

  def sameTree(p: TreeNode, q: TreeNode): Boolean = {
    /*
      Given the roots of two binary trees p and q, write a function to check if they are the same or not.
      Two binary trees are considered the same if they are structurally identical, and the nodes have the same value.
      Breadth-first search using a MutableQueue of tuples
      Worst time complexity: O(N / 2)
      Space complexity: O(N)
    */
    @tailrec
    def bfsHelper(queue: scala.collection.immutable.Queue[(TreeNode, TreeNode)]): Boolean = {
      if (queue.isEmpty) true
      else queue.dequeue match {
        case ((null, null), newQueue) => bfsHelper(newQueue)
        case ((null, _), _) => false
        case ((_, null), _) => false
        case ((x, y), newQueue) if x.value == y.value => bfsHelper(newQueue.enqueue((x.left, y.left)).enqueue((x.right, y.right)))
        case _ => false
      }
    }
    bfsHelper(scala.collection.immutable.Queue((p, q)))
  }

  def cloneGraph(graph: Node): Node = {
    /*
      Given a reference of a node in a connected undirected graph, return a deep copy (clone) of the graph.
      Each node in the graph contains a value (int) and a list (List[Node]) of its neighbors.
      The graph is represented in the test case using an adjacency list.
      An adjacency list is a collection of unordered lists used to represent a finite graph. Each list describes the set of neighbors of a node in the graph.
      The given node will always be the first node with val = 1. You must return the copy of the given node as a reference to the cloned graph.
    */
    val visited = scala.collection.mutable.Map[Node, Node]()
    val queue = scala.collection.mutable.Queue[Node]()
    @tailrec
    def bfsHelper(): Node = {
      if (queue.isEmpty) visited(graph) else {
        val node = queue.dequeue
        node.neighbors foreach {neighbor =>
          if (!visited.contains(neighbor)) {
            visited addOne (neighbor -> new Node(neighbor.value))
            queue addOne neighbor
          }
          visited(node).neighbors = visited(node).neighbors prepended visited(neighbor)
        }
        bfsHelper()
      }
    }
    if (graph == null) graph else {
      queue addOne graph
      visited addOne (graph -> new Node(graph.value))
      bfsHelper()
    }
  }

  def numberIslands(grid: Array[Array[Char]]): Int = {
    /*
      Given an m x n 2D binary grid grid which represents a map of '1's (land) and '0's (water), return the number of islands.
      An island is surrounded by water and is formed by connecting adjacent lands horizontally or vertically.
      You may assume all four edges of the grid are all surrounded by water.
      Time complexity: O(N * M)
      Worst Space complexity: O(N * M) due to recursion stack depth
    */
    val (height, width) = (grid.length, grid(0).length)
    val directions = List((-1, 0), (1, 0), (0, -1), (0, 1))

    def dfsHelper(i: Int, j: Int): Unit = {
      if (i >= 0 && j >= 0 && i < height && j < width && grid(i)(j) != '0') {
        grid(i)(j) = '0' // every visited node should be set as '0' to mark as visited
        directions foreach { case (x, y) => dfsHelper(i + x, j + y) }
      }
    }
    var islands = 0
    for {
      i <- 0 until height
      j <- 0 until width
      if grid(i)(j) == '1'
    } {
      // Count the # of root nodes that trigger DFS, this # is the # of islands since each DFS starting at some root identifies an island.
      islands += 1
      dfsHelper(i, j)
    }
    islands
  }
}

