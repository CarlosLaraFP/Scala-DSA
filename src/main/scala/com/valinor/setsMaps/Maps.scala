package com.valinor.setsMaps

import scala.annotation.tailrec
import scala.collection.immutable.{Map => ImmutableMap}
import scala.collection.mutable.{Map => MutableMap}

sealed trait HashMap[A, B] {
  protected def put(key: A, value: B): Unit
  protected def get(key: A): Option[B]
  protected def delete(key: A): Unit
  protected def size: Int
  protected def hash(key: A): Int
}

final class Map[A, B](initialSize: Int) extends HashMap[A, B] {
  /*
  */
  override def put(key: A, value: B): Unit = {
    /*
      Our hash function guarantees bucketIndex is within bounds of the mutable bucket Array
      Time complexity: O(1) <- eC
    */
    val bucketIndex: Int = getBucketIndex(key)

    @tailrec
    def exists(index: Int, bucket: List[(A, B)]): Boolean = bucket.headOption match {
      case None => false
      case Some(x) if x._1 == key =>
        _bucketArray.update(bucketIndex, _bucketArray(bucketIndex).updated(index, (key, value)))
        true
      case _ => exists(index + 1, bucket.tail)
    }
    // Put
    if (!exists(0, _bucketArray(bucketIndex))) {
      _bucketArray.update(bucketIndex, (key, value) :: _bucketArray(bucketIndex))
      _entries += 1
    }
    // Load factor check => re-distribution
    if (_entries.toFloat / _bucketArray.length > 0.7) {
      _entries = 0
      _rehash()
    }
  }

  override def get(key: A): Option[B] = {
    /*
      Time complexity: O(1) <- eC
    */
    val bucketIndex: Int = getBucketIndex(key)
    val value = _bucketArray(bucketIndex).find(_._1 == key)
    if (value.isEmpty) None else Some(value.get._2)
  }

  override def delete(key: A): Unit = get(key) match {
    /*
      Time complexity: O(1) <- eC
    */
    case None =>
    case _ =>
      val bucketIndex = getBucketIndex(key)
      _bucketArray.update(bucketIndex, _bucketArray(bucketIndex).filter(_._1 != key))
  }

  private var _entries = 0
  override def size: Int = _entries

  private var _bucketArray: Array[List[(A, B)]] = Array.fill(initialSize)(Nil)
  def bucketArray: Array[List[(A, B)]] = _bucketArray

  protected def hash(key: A): Int = {
    /*
      Must return unique values for unique objects (else, collisions).
      For a string, say `abcde`, a very effective function is treating this as number of prime number base `p`.
      a * p^4 + b * p^3 + c * p^2 + d * p^1 + e * p^0
      We replace each character with its corresponding ASCII value.
      We use prime numbers because the provide a good distribution (most common are 31 and 37).
    */
    val k = key.toString
    val compressionFactor = _bucketArray.length
    @tailrec
    def hashHelper(index: Int, hashCode: Int, currentCoefficient: Int): Int = k.lift(index) match {
      case None => hashCode % compressionFactor
      case Some(x) =>
        hashHelper(index + 1, (hashCode + (x.toInt * currentCoefficient)) % compressionFactor, (currentCoefficient * 31) % compressionFactor)
    }
    hashHelper(0, 0, 1)
  }

  def getBucketIndex(key: A): Int = hash(key)

  private def _rehash(): Unit = {
    val oldBucketArray = _bucketArray
    _bucketArray = Array.fill(oldBucketArray.length * 2)(Nil)
    oldBucketArray foreach { _.foreach(entry => put(entry._1, entry._2)) }
  }
}

final class HashTable {
  /*
    Hash Table vs Hash Map
    A hash table stores values and a hash map stores key-value pairs.

    **Assumptions**
    1. The string will have at least two letters,
    2. The first two characters are uppercase letters (ASCII values from 65 to 90).

    **Rules**
    Do not use a Map - only IndexedSeq
    Store `lists` at each bucket, and not just the string itself. For example, you can store "UDACITY" at index 8568 as ["UDACITY"].
  */
  private val _bucketArray: Array[List[String]] = Array.fill(9000)(Nil)

  def store(input: String): Unit = {
    /*
      Takes string as input and stores it in a hash table, if not already present.
      O(1) worst time complexity.
    */
    if (lookup(input) == -1) {
      val bucketIndex = calculateHashValue(input)
      _bucketArray.update(bucketIndex, _bucketArray(bucketIndex) prepended input)
    }
  }

  def lookup(input: String): Int = {
    /*
      Checks if a string is already available in the hash table.
      If yes, return the value, else return -1.
      Effectively constant time complexity.
    */
    val bucketIndex = calculateHashValue(input)
    if (_bucketArray(bucketIndex).contains(input)) bucketIndex else -1
  }

  def calculateHashValue(input: String): Int = {
    /*
      Helper method to calculate the hash value of a given string.
      The generated hash value is the bucket index.
      Constant time complexity.
    */
    val chars = input.take(2).map(_.toInt)
    chars(0) * 100 + chars(1)
  }
}

object MapAlgorithms {
  /*
    Pair Sum To Target Algorithm

    Given an `input_list` and a `target`, return the pair of indices in the list that holds the values which sum to the target.
    1. The best solution takes O(n) time. *This means that you cannot traverse the given list more than once.* **Hint - Think of an additional data structure that you should use here.**
    2. You can assume that the list does not have any duplicates.
  */
  def pairSumToTarget(input: Array[Int], target: Int): List[(Int, Int)] = {
    /*
      Time complexity: O(N)
      We could store index and value in a Map
      O(2N) WC is easy
    */
    @tailrec
    def sumHelper(index: Int, map: ImmutableMap[Int, Int], output: List[(Int, Int)]): List[(Int, Int)] = {
      if (index >= input.length) output
      else {
        val currentElement = input(index)
        //val targetMatch = map.find(_._2 + currentElement == target)
        val targetMatch = map.get(target - currentElement)
        if (targetMatch.isDefined)
          sumHelper(index + 1, map + (currentElement -> index), (targetMatch.get, index) :: output)
        else
          sumHelper(index + 1, map + (currentElement -> index), output)
      }
    }
    sumHelper(0, ImmutableMap[Int, Int](), Nil)
  }

  def longestSubSequenceU(input: Array[Int]): Vector[Int] = {
    /*
      Given a list of integers that contain natural numbers in random order,
      find the longest possible sub sequence of consecutive numbers in the array. Return this subsequence in sorted order.
      Given the list `5, 4, 7, 10, 1, 3, 55, 2`, the output should be `1, 2, 3, 4, 5`
      If two subsequences are of equal length, return the subsequence whose index of smallest element comes first.
    */
    def indices(seq: Vector[Int], longest: List[(Int, Int)]): List[(Int, Int)] = {
      if (longest.isEmpty || seq.size == longest.head._2) (seq.head, seq.size) :: longest
      else if (seq.size > longest.head._2) List((seq.head, seq.size))
      else longest
    }
    @tailrec
    def sequenceHelper(index: Int, map: ImmutableMap[Int, Vector[Int]], longest: List[(Int, Int)]): Vector[Int] = {
      if (index == input.length) map(longest.min._1)
      else if (map.contains(input(index))) sequenceHelper(index + 1, map, longest)
      else  {
        val x = input(index)
        val larger = map.get(x + 1)
        val smaller = map.get(x - 1)
        if (larger.isDefined && smaller.isDefined) {
          val seq = (smaller.get :+ x) ++ larger.get
          sequenceHelper(index + 1, map.updated(seq.head, seq).updated(seq.last, seq).updated(x, Vector()), indices(seq, longest))
        }
        else if (larger.isDefined && smaller.isEmpty) {
          val seq = x +: larger.get
          sequenceHelper(index + 1, map.updated(seq.last, seq) + ((seq.head, seq)), indices(seq, longest))
        }
        else if (larger.isEmpty && smaller.isDefined) {
          val seq = smaller.get :+ x
          sequenceHelper(index + 1, map.updated(seq.head, seq) + ((seq.last, seq)), indices(seq, longest))
        }
        else sequenceHelper(index + 1, map + ((x, Vector(x))), indices(Vector(x), longest))
      }
    }
    if (input.isEmpty) Vector() else sequenceHelper(0, ImmutableMap(), List())
  }

  def lengthSubSequence(input: Array[Int]): Int = {
    /*
      Given an unsorted array of integers,
      return the length of the longest consecutive elements sequence.
      5, 4, 9, 6, 11, 2, 3, 13, 1 == 6
    */
    val set = input.toSet
    @tailrec
    def searchHelper(currentNumber: Int, currentStreak: Int, longestStreak: Int): Int = {
      if (set.contains(currentNumber + 1)) searchHelper(currentNumber + 1, currentStreak + 1, longestStreak)
      else scala.math.max(currentStreak, longestStreak)
    }
    var longestStreak = 0
    // Only proceed if it's the head of a sub sequence. Then, increment until last. Store longest length.
    set.foreach(n => if (!set.contains(n - 1)) longestStreak = searchHelper(n, 1, longestStreak))
    longestStreak
  }

  def longestSubSequence(input: Array[Int]): Vector[Int] = {
    /*
      5, 4, 9, 6, 11, 2, 3, 13, 1 == 6
    */
    val set = input.toSet
    @tailrec
    def searchHelper(currentNumber: Int, currentStreak: Int, longestLast: Int, longestStreak: Int): (Int, Int) = {
      if (set.contains(currentNumber + 1)) searchHelper(currentNumber + 1, currentStreak + 1, longestLast, longestStreak)
      else if (currentStreak > longestStreak) (currentNumber, currentStreak)
      else if (currentStreak == longestStreak && currentNumber < longestLast) (currentNumber, currentStreak)
      else (longestLast, longestStreak)
    }
    var longest = (0, 0)
    // Only proceed if it's the head of a sub sequence. Then, increment until last. Store longest length.
    set.foreach(n => if (!set.contains(n - 1)) longest = searchHelper(n, 1, longest._1, longest._2))
    if (longest._2 == 0) Vector() else (longest._1 - longest._2 + 1 to longest._1).toVector
  }
}

