import com.valinor.LeetCode
import com.valinor.stacksQueues.{IndexedQueue, IndexedStack, LinearQueue, LinearStack, StackedQueue}
import com.valinor.stacksQueues.StackQueueAlgorithms._
import com.valinor.lists.{DoublyLinkedList, NestedSortedLinkedList, SortedLinkedList}
import com.valinor.lists.LinkedListAlgorithms._
import com.valinor.recursion.RecursionAlgorithms._
import com.valinor.trees.TreeAlgorithms._
import com.valinor.trees.{BinarySearchTree, Node, Tree}
import com.valinor.setsMaps.{HashTable, Map}
import com.valinor.setsMaps.MapAlgorithms._
import com.valinor.LeetCode.LeetCodeAlgorithms._
import com.valinor.LeetCode.{ListNode, Node => LeetCodeNode, TreeNode}

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.{Queue => ImmutableQueue}
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

class AlgorithmsSpec extends AnyFlatSpec with Matchers {
  /*
    Using Scalatest for systematic unit testing.
  */

  // Udacity

  "A custom mutable doubly Linked List" should "behave as expected" in {
    val testList = new DoublyLinkedList[Int]()
    testList prepend 2
    testList.head.get.value shouldEqual 2
    testList.head.get.previous shouldEqual None
    testList.head.get.next shouldEqual None
    testList.tail.get.value shouldEqual 2
    testList.tail.get.previous shouldEqual None
    testList.tail.get.next shouldEqual None
    testList append 4
    testList.head.get.value shouldEqual 2
    testList.head.get.previous shouldEqual None
    testList.head.get.next.get.value shouldEqual 4
    testList.tail.get.value shouldEqual 4
    testList.tail.get.previous.get.value shouldEqual 2
    testList.tail.get.next shouldEqual None
    testList prepend 1
    testList.head.get.previous shouldEqual None
    testList.head.get.value shouldEqual 1
    testList.head.get.next.get.value shouldEqual 2
    testList.head.get.next.get.previous.get.value shouldEqual 1
    testList.head.get.next.get.next.get.value shouldEqual 4
    testList.tail.get.previous.get.value shouldEqual 2
    testList.tail.get.value shouldEqual 4
    testList.tail.get.next shouldEqual None

    testList search 0 shouldEqual None
    testList search -1 shouldEqual None
    (testList search 1).get.value shouldEqual 1
    (testList search 1).get.previous shouldEqual None
    (testList search 1).get.next.get.value shouldEqual 2
    (testList search 4).get.value shouldEqual 4
    (testList search 4).get.previous.get.value shouldEqual 2
    (testList search 4).get.next shouldEqual None
    val searchList = new DoublyLinkedList[Int]()
    searchList search 1 shouldEqual None

    //testList.getElements.foreach(println)
    testList remove 0
    testList.size shouldEqual 3
    testList remove 1
    testList.head.get.value shouldEqual 2
    testList.tail.get.value shouldEqual 4
    //testList.getElements.foreach(println)
    testList remove 4
    testList.head.get.value shouldEqual 2
    testList.tail.get.value shouldEqual 2
    //testList.getElements.foreach(println)

    val newList = new DoublyLinkedList[Int]()
    newList.pop shouldEqual None
    newList append 1
    newList.size shouldEqual 1
    newList.pop shouldEqual Some(1)
    newList.size shouldEqual 0
    newList append 2
    newList append 4
    newList.pop shouldEqual Some(2)
    newList.size shouldEqual 1

    val listA = new DoublyLinkedList[String]()
    listA append "4"
    listA prepend "2"
    listA prepend "1"
    listA.insert("-1", 0)
    listA.size shouldEqual 4
    listA.head.get.value shouldEqual "-1"
    listA.head.get.next.get.value shouldEqual "1"
    listA.insert("8", -1)
    listA.tail.get.value shouldEqual "8"
    listA.tail.get.previous.get.value shouldEqual "4"
    listA.size shouldEqual 5
    listA.getElements shouldEqual Vector("-1", "1", "2", "4", "8")

    val listB = listA.reverse()
    listB.getElements shouldEqual Vector("8", "4", "2", "1", "-1")
    listA.pop()
    listA.pop()
    listA.pop()
    listA.pop()
    listA.reverse().getElements shouldEqual Vector("8")
    listA.pop()
    listA.reverse().size shouldEqual 0

    listB.isCircular shouldEqual false
    val circularList = new DoublyLinkedList[Int]()
    circularList prepend 5
    circularList prepend 3
    circularList.createCircularRelation()
    circularList.isCircular shouldEqual true
  }

  "A custom mutable sorted Linked List" should "behave as expected" in {
    val list = new SortedLinkedList()
    list append 4
    list append 2
    list.head.get.value shouldEqual 2
    list.head.get.next.get.value shouldEqual 4
    list append 8
    list.head.get.value shouldEqual 2
    list.head.get.next.get.value shouldEqual 4
    list.head.get.next.get.next.get.value shouldEqual 8

    val integers = Vector(7, 3, -4, 9, 1, -1, 3)
    val sortedIntegers = list sortSequence integers
    sortedIntegers shouldEqual Vector(-4, -1, 1, 3, 3, 7, 9)
  }

  "A custom mutable nested sorted Linked List" should "be flattened" in {
    // Create 3 SortedLinkedLists and append
    val listA = new SortedLinkedList()
    listA append 1
    listA append 2
    listA append 3
    //println(listA.getElements)
    val listB = new SortedLinkedList()
    listB append 9
    listB append 8
    listB append 7
    //println(listB.getElements)
    val listC = new SortedLinkedList()
    listC append 5
    listC append 4
    listC append 6
    //println(listC.getElements)

    val nestedList = new NestedSortedLinkedList()
    nestedList prepend listC
    nestedList prepend listA
    nestedList prepend listB
    //println(nestedList.head.get.value.getElements)
    //println(nestedList.head.get.next.get.value.getElements)
    //println(nestedList.head.get.next.get.next.get.value.getElements)

    val flattenedList = nestedList.flatten
    flattenedList.getElements shouldEqual Vector(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  "Add One algorithm" should "return array with incremented number" in {
    // 6 test cases
    val inputC = Vector[Int]()
    val outputC = Vector(1)
    addOne(inputC) shouldEqual outputC
    val inputD = Vector(0)
    val outputD = Vector(1)
    addOne(inputD) shouldEqual outputD
    val inputE = Vector(9)
    val outputE = Vector(1, 0)
    addOne(inputE) shouldEqual outputE
    val inputF = Vector(9, 9, 9)
    val outputF = Vector(1, 0, 0, 0)
    addOne(inputF) shouldEqual outputF
    val inputA = Vector(1, 2, 3)
    val outputA = Vector(1, 2, 4)
    addOne(inputA) shouldEqual outputA
    val inputB = Vector(2, 4, 6, 9)
    val outputB = Vector(2, 4, 7, 0)
    addOne(inputB) shouldEqual outputB
  }

  "Duplicate Number algorithm" should "return duplicate element in array" in {
    val inputB = Vector(0, 0)
    findDuplicateNumber(inputB) shouldEqual 0
    val inputA = Vector(0, 2, 3, 1, 4, 5, 3)
    findDuplicateNumber(inputA) shouldEqual 3
    val inputC = Vector(6, 0, 2, 1, 4, 5, 3, 6)
    findDuplicateNumber(inputC) shouldEqual 6
    // Immutable [Linked] List is technically faster
    val inputD = List(0, 0)
    findDuplicateNumber(inputD) shouldEqual 0
    val inputE = List(0, 2, 3, 1, 4, 5, 3)
    findDuplicateNumber(inputE) shouldEqual 3
    val inputF = List(6, 0, 2, 1, 4, 5, 3, 6)
    findDuplicateNumber(inputF) shouldEqual 6
  }

  "Max Sum Sub Array algorithm" should "return largest sum in sub array" in {
    val inputA = List(1, 2, 3, -4, 6)
    maxSumSubArray(inputA) shouldEqual 8
    val inputB = List(1, 2, -5, -4, 1, 6)
    maxSumSubArray(inputB) shouldEqual 7
    val inputC = List()
    maxSumSubArray(inputC) shouldEqual 0
    val inputD = List(3, 6, 9, 12, -4, -8, -1)
    maxSumSubArray(inputD) shouldEqual 30
    val inputE = List(-2, 1, -3, 4, -1, 2, 1, -5, 4)
    maxSumSubArray(inputE) shouldEqual 6
    val inputF = List(-2, 1, -3)
    maxSumSubArray(inputF) shouldEqual 1
    val inputG = List(-2, 1, -3, 4, -1, 2, 1, -5, 4, 11)
    maxSumSubArray(inputG) shouldEqual 16
    val inputH = List(-2, 1, -3, 4, -1, 2, 1, -6, 4, 12)
    maxSumSubArray(inputH) shouldEqual 16
    val inputX = List(-2, 1)
    maxSumSubArray(inputX) shouldEqual 1
    val inputY = List(-1)
    maxSumSubArray(inputY) shouldEqual -1

    val input1 = Array(1, 2, 3, -4, 6)
    maxSumSubArrayLC(input1) shouldEqual 8
    val input2 = Array(1, 2, -5, -4, 1, 6)
    maxSumSubArrayLC(input2) shouldEqual 7
    val input3 = Array[Int]()
    maxSumSubArrayLC(input3) shouldEqual 0
    val input4 = Array(3, 6, 9, 12, -4, -8, -1)
    maxSumSubArrayLC(input4) shouldEqual 30
    val input5 = Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)
    maxSumSubArrayLC(input5) shouldEqual 6
    val input6 = Array(-2, 1, -3)
    maxSumSubArrayLC(input6) shouldEqual 1
    val input7 = Array(-2, 1, -3, 4, -1, 2, 1, -5, 4, 11)
    maxSumSubArrayLC(input7) shouldEqual 16
    val input8 = Array(-2, 1, -3, 4, -1, 2, 1, -6, 4, 12)
    maxSumSubArrayLC(input8) shouldEqual 16
    val input9 = Array(-2, 1)
    maxSumSubArrayLC(input9) shouldEqual 1
    val input10 = Array(-1)
    maxSumSubArrayLC(input10) shouldEqual -1
  }

  "Pascal's Triangle algorithm" should "return nth row of Pascal triangle" in {
    nthRowPascal(0) shouldEqual List(1)
    nthRowPascal(1) shouldEqual List(1, 1)
    nthRowPascal(2) shouldEqual List(1, 2, 1)
    nthRowPascal(3) shouldEqual List(1, 3, 3, 1)
    nthRowPascal(4) shouldEqual List(1, 4, 6, 4, 1)
    nthRowPascal(5) shouldEqual List(1, 5, 10, 10, 5, 1)
  }

  "Odd Even Nodes algorithm" should "return List with reordered elements" in {
    oddEvenNodes(List(1, 2, 3, 4, 5)) shouldEqual List(1, 3, 5, 2, 4)
    oddEvenNodes(List(1, 2, 3, 4, 5, 6)) shouldEqual List(1, 3, 5, 2, 4, 6)
    oddEvenNodes(List(2, 1, 3, 5, 6, 4, 7)) shouldEqual List(2, 3, 6, 7, 1, 5, 4)
    oddEvenNodes(List[Int]()) shouldEqual List[Int]()
    oddEvenNodes(List[Int](2)) shouldEqual List[Int](2)
    oddEvenNodes(List(5, 5, 4, 4, 1)) shouldEqual List(5, 4, 1, 5, 4)
  }

  "Skip i Delete j algorithm" should "return array with elements removed" in {
    skipIDeleteJ(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 2, 3) shouldEqual List(1, 2, 6, 7, 11, 12)
    skipIDeleteJ(List[Int](), 8, 9) shouldEqual List[Int]()
    skipIDeleteJ(List(1, 3, 5, 7, 9), 1, 1) shouldEqual List(1, 5, 9)
    skipIDeleteJ(List(1, 3, 5, 7, 9), 1, 0) shouldEqual List(1, 3, 5, 7, 9)
    skipIDeleteJ(List(1, 3, 5, 7), 0, 1) shouldEqual List[Int]()
    skipIDeleteJ(List(1, 3, 5, 7, 11, 13), 0, 2) shouldEqual List[Int]()
    skipIDeleteJ(List(1, 3, 5, 7, 11, 13, 19), 0, 20) shouldEqual List[Int]()
    skipIDeleteJ(List(1, 3, 5, 7, 11, 13, 19), 1, 5) shouldEqual List(1, 19)
  }

  "Swap Nodes algorithm" should "return List with swapped elements" in {
    swapNodes(List(3, 4, 5, 2, 6, 1, 9), 2, 5) shouldEqual List(3, 4, 1, 2, 6, 5, 9)
    swapNodes(List(3, 4, 5, 2, 6, 1, 9), 0, 0) shouldEqual List(3, 4, 5, 2, 6, 1, 9)
    swapNodes(List(3, 4, 5, 2, 6, 1, 9), 3, 3) shouldEqual List(3, 4, 5, 2, 6, 1, 9)
    swapNodes(List(3, 4, 5, 2, 6, 1, 9), 10, 16) shouldEqual List(3, 4, 5, 2, 6, 1, 9)
    swapNodes(List(3, 4, 5, 2, 6, 1, 9), 0, 10) shouldEqual List(9, 4, 5, 2, 6, 1, 3)
    swapNodes(List(9, 8, 7, 6), 1, 3) shouldEqual List(9, 6, 7, 8)
    swapNodes(List(9, 8, 7, 6), 1, 4) shouldEqual List(9, 6, 7, 8)
  }

  "A linear Stack" should "push & pop values in LIFO order" in {
    val stack = new LinearStack[Int]()
    stack.elements shouldEqual List[Int]()
    stack.numElements shouldEqual 0
    stack push -1
    stack.elements shouldEqual List(-1)
    stack.numElements shouldEqual 1
    stack.pop() shouldEqual Some(-1)
    stack.elements shouldEqual List[Int]()
    stack.numElements shouldEqual 0
    stack push 10
    stack push 20
    stack push 30
    stack push 40
    stack push 50
    stack.numElements shouldEqual 5
    stack.pop() shouldEqual Some(50)
    stack push 60
    stack.pop() shouldEqual Some(60)
    stack.pop() shouldEqual Some(40)
    stack.pop() shouldEqual Some(30)
    stack push 50
    stack.numElements shouldEqual 3
  }

  "An indexed Stack" should "push & pop values in LIFO order" in {
    val stack = new IndexedStack[Int]()
    stack.array shouldEqual ArrayBuffer[Int]()
    stack.numElements shouldEqual 0
    stack push -1
    stack.array shouldEqual ArrayBuffer(-1)
    stack.numElements shouldEqual 1
    stack.pop() shouldEqual Some(-1)
    stack.array shouldEqual ArrayBuffer[Int]()
    stack.numElements shouldEqual 0
  }

  "Balance Parentheses algorithm" should "balance parentheses in expression" in {
    val equationE = "(n + m)"
    balancedParentheses(equationE) shouldEqual true
    val equationC = "))3*x("
    balancedParentheses(equationC) shouldEqual false
    val equationA = "((3^2 + 8)*(5/2))/(2+6)"
    balancedParentheses(equationA) shouldEqual true
    val equationB = "((3^2 + 8)*(5/2))/(2+6))"
    balancedParentheses(equationB) shouldEqual false
    val equationD = "((3x + 1)))(y + 2)"
    balancedParentheses(equationD) shouldEqual false
    val equationF = "))3*x(("
    balancedParentheses(equationF) shouldEqual false
    val equationG = "3 + x + m"
    balancedParentheses(equationG) shouldEqual true
  }

  "Reverse Polish algorithm" should "compute result from reverse Polish expression" in {
    reversePolish(Array("3", "1", "+", "4", "*")) shouldEqual Left(16)
    reversePolish(Array("3", "1", "*", "4", "-")) shouldEqual Left(-1)
    reversePolish(Array("10", "2", "/", "5", "/")) shouldEqual Left(1)
    reversePolish(Array("10", "2", "/", "4", "/")) shouldEqual Left(1)
    reversePolish(Array("10", "2", "/", "6", "/")) shouldEqual Left(0)
    reversePolish(Array("2", "/", "2")) shouldEqual Right("Invalid element in input array at index 1")
    reversePolish(Array("2", "/")) shouldEqual Right("Input array requires a minimum of 3 elements")
    reversePolish(Array("2", "1", "0")) shouldEqual Right("Invalid element in input array at index 2")
    reversePolish(Array("+", "1", "0")) shouldEqual Right("Invalid element in input array at index 0")
    reversePolish(Array("8", "9", "@")) shouldEqual Right("Invalid element in input array at index 2")
  }

  "Reverse Stack algorithm" should "reverse a Linked List / linear Stack" in {
    reverseStack(List(1, 2, 3, 4)) shouldEqual List(4, 3, 2, 1)
    reverseStack(List(1)) shouldEqual List(1)
    reverseStack(List(-1, -1)) shouldEqual List(-1, -1)
  }

  "Minimum Reversals algorithm" should "return number of reversals to balance brackets" in {
    // two ways to re-arrange brackets for the same valid result
    minReversalsBalanced("}}}}") shouldEqual 2
    minReversalsBalanced("{{{{") shouldEqual 2
    minReversalsBalanced("}{}}") shouldEqual 1
    minReversalsBalanced("{{}}") shouldEqual 0
    // 36 characters, 31 and 5, 26 and 10 ; never adding or removing (fixed string size)
    minReversalsBalanced("{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}") shouldEqual 13
    minReversalsBalanced("{{}") shouldEqual -1
    minReversalsBalanced("{}{}") shouldEqual 0
    minReversalsBalanced("}{") shouldEqual 2
    minReversalsBalanced("}}{{") shouldEqual 2
    minReversalsBalanced("}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{") shouldEqual 2
    minReversalsBalanced("{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{") shouldEqual 2
    minReversalsBalanced("{}{}{}{}{}{}{}{}{}{}}}{}{}{}{}{{") shouldEqual 2
  }

  "Mutable Indexed Queue" should "behave as FIFO queue" in {
    //
    val indexedQueue = new IndexedQueue[Int]()
    indexedQueue enqueue 10
    indexedQueue enqueue 9
    indexedQueue enqueue 8
    indexedQueue enqueue 7
    indexedQueue.size shouldEqual 4
    indexedQueue.front shouldEqual Some(10)
    indexedQueue.dequeue() shouldEqual Some(10)
    indexedQueue.size shouldEqual 3
    indexedQueue.front shouldEqual Some(9)
    indexedQueue.dequeue() shouldEqual Some(9)
    indexedQueue.size shouldEqual 2
    indexedQueue.front shouldEqual Some(8)
    indexedQueue.dequeue() shouldEqual Some(8)
    indexedQueue.size shouldEqual 1
    indexedQueue.front shouldEqual Some(7)
  }

  "Mutable Linear Queue" should "behave as FIFO queue" in {
    //
    val linearQueue = new LinearQueue[Int]()
    linearQueue enqueue 10
    linearQueue enqueue 9
    linearQueue enqueue 8
    linearQueue enqueue 7
    linearQueue.size shouldEqual 4
    linearQueue.front shouldEqual Some(10)
    linearQueue.dequeue() shouldEqual Some(10)
    linearQueue.size shouldEqual 3
    linearQueue.front shouldEqual Some(9)
    linearQueue.dequeue() shouldEqual Some(9)
    linearQueue.size shouldEqual 2
    linearQueue.front shouldEqual Some(8)
    linearQueue.dequeue() shouldEqual Some(8)
    linearQueue.size shouldEqual 1
    linearQueue.front shouldEqual Some(7)
  }

  "Mutable Stacked Queue" should "behave as FIFO queue" in {
    //
    val stackedQueue = new StackedQueue[Int]()
    stackedQueue enqueue 10
    stackedQueue enqueue 9
    stackedQueue enqueue 8
    stackedQueue enqueue 7
    stackedQueue.size shouldEqual 4
    stackedQueue.front shouldEqual Some(10)
    stackedQueue.dequeue() shouldEqual Some(10)
    stackedQueue.size shouldEqual 3
    stackedQueue.front shouldEqual Some(9)
    stackedQueue.dequeue() shouldEqual Some(9)
    stackedQueue.size shouldEqual 2
    stackedQueue.front shouldEqual Some(8)
    stackedQueue.dequeue() shouldEqual Some(8)
    stackedQueue.size shouldEqual 1
    stackedQueue.front shouldEqual Some(7)
  }

  "Reverse Queue algorithm" should "return reversed immutable Queue" in {
    //
    reverseQueue(ImmutableQueue(1, 2, 3, 4, 5)) shouldEqual ImmutableQueue(5, 4, 3, 2, 1)
    reverseQueue(ImmutableQueue(10)) shouldEqual ImmutableQueue(10)
  }

  "Factorial algorithm" should "return recursive factorial" in {
    //
    factorial(-1) shouldEqual None
    factorial(0) shouldEqual Some(1)
    factorial(1) shouldEqual Some(1)
    factorial(2) shouldEqual Some(2)
    factorial(3) shouldEqual Some(6)
    factorial(4) shouldEqual Some(24)
    factorial(5) shouldEqual Some(120)
  }

  "Reverse String algorithm" should "reverse string recursively" in {
    //
    reverseString("") shouldEqual ""
    reverseString("A") shouldEqual "A"
    reverseString("Type") shouldEqual "epyT"
    reverseString("abcba") shouldEqual "abcba"
    reverseString("hello") shouldEqual "olleh"
  }

  "Check Palindrome algorithm" should "return a boolean option" in {
    //
    checkPalindrome("") shouldEqual true
    checkPalindrome("madam") shouldEqual true
    checkPalindrome("Cat") shouldEqual false
    checkPalindrome("A") shouldEqual true
    checkPalindrome("aaa") shouldEqual true
    checkPalindrome("helloolleh") shouldEqual true
    checkPalindrome("test") shouldEqual false
  }

  "Add One Recursive algorithm" should "return sequence with new number" in {
    //
    addOneRec(Vector(1, 2, 3)) shouldEqual Vector(1, 2, 4)
    addOneRec(Vector(1, 2, 9)) shouldEqual Vector(1, 3, 0)
    addOneRec(Vector(9, 9, 9)) shouldEqual Vector(1, 0, 0, 0)
    addOneRec(Vector(3)) shouldEqual Vector(4)
  }

  "List Permutations algorithm" should "return sequence of permutations" in {
    findPermutationsI(Array(2)) shouldEqual List(List(2))
    findPermutationsI(Array(1, 2)) shouldEqual List(List(2, 1), List(1, 2))
    findPermutationsI(Array(0, 1, 2)) should contain allOf (List(0, 1, 2), List(0, 2, 1), List(1, 0, 2), List(1, 2, 0), List(2, 0, 1), List(2, 1, 0))
  }

  "String Permutations algorithm" should "return sequence of permutations" in {
    stringPermutations("A") shouldEqual List("A")
    stringPermutations("AB") shouldEqual List("BA", "AB")
    stringPermutations("012") should contain allOf ("012", "021", "102", "120", "201", "210")
    stringPermutations("219") should contain allOf ("912", "921", "192", "129", "291", "219")
  }

  "Keypad Combinations algorithm" should "return list of string combinations" in {
    //
    keypadCombinations(2) should contain allOf ("a", "b", "c")
    keypadCombinations(23) should contain allOf ("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf")
    keypadCombinations(32) should contain allOf ("da", "db", "dc", "ea", "eb", "ec", "fa", "fb", "fc")
    keypadCombinations(29) should contain allOf ("aw", "ax", "ay", "az", "bw", "bx", "by", "bz", "cw", "cx", "cy", "cz")
    keypadCombinations(232) should contain allOf ("ada", "aea", "afa", "bda", "bea", "bfa", "cda", "cea", "cfa", "adb", "aeb", "afb", "bdb", "beb", "bfb", "cdb", "ceb", "cfb", "adc", "aec", "afc", "bdc", "bec", "bfc", "cdc", "cec", "cfc")
  }

  "Deep Reverse algorithm" should "return fully reversed list" in {
    //
    deepReverse(List(2, 4, 6, 8, 10)) shouldEqual List(10, 8, 6, 4, 2)
    deepReverse(List(1, 2, List(3, 4, 5), 4, 5)) shouldEqual List(5, 4, List(5, 4, 3), 2, 1)
    deepReverse(List(1, List(2, 3, List(4, List(5, 6))))) shouldEqual List(List(List(List(6, 5), 4), 3, 2), 1)
  }

  "Binary Search algorithm" should "return target's index" in {
    //
    binarySearch(Vector(1, 2, 3, 4, 5), 3) shouldEqual 2
    binarySearch(Vector(1, 2, 3, 4, 5), 1) shouldEqual 0
    binarySearch(Vector(1, 2, 3, 4, 5), 5) shouldEqual 4
    binarySearch(Vector(1, 2, 3, 4), 3) shouldEqual 2
    binarySearch(Vector(1, 2, 3, 4), 2) shouldEqual 1
    binarySearch(Vector(2, 4, 6), 3) shouldEqual -1
    binarySearch(Vector(1, 2, 3, 4), 7) shouldEqual -1
    binarySearch(Vector(1, 2, 3, 4), 0) shouldEqual -1
  }

  "Tower of Hanoi algorithm" should "print steps" in {
    // On average, O(3N)
    towerHanoi(1) shouldEqual Vector("SD")
    towerHanoi(2) shouldEqual Vector("SD", "SA", "DS", "AD", "SD")
    towerHanoi(3) shouldEqual Vector("SD", "SA", "DA", "SD", "AS", "AD", "SD")
    towerHanoi(4) shouldEqual Vector("SD", "SA", "DA", "SD", "AS", "AD", "SD", "SA", "DS", "AD", "SD")
    towerHanoi(5) // 13
    //println(towerHanoi(6).size) // 17
  }

  "Return Codes algorithm" should "return combinations" in {
    //
    returnCodes(1) should contain ("a")
    returnCodes(99) should contain ("ii")
    returnCodes(11) should contain allOf ("aa", "k")
    returnCodes(123) should contain allOf ("abc", "aw", "lc")
    returnCodes(339) should contain ("cci")
    returnCodes(1226) should contain allOf ("abbf", "lbf", "avf", "abz")
    returnCodes(999999) should contain ("iiiiii")
    returnCodes(145) should contain allOf ("ade", "ne")
    // 1
    //println(1 % 10)
    // 0
    //println(1 / 10)
  }

  "Find Subsets algorithm" should "pass all tests" in {
    //
    val x = findSubsets(Array(9, 9))
    x.size shouldEqual 4
    List(List(), List(9), List(9), List(9, 9)).foreach(x should contain (_))

    val y = findSubsets(Array(9, 12, 15))
    y.size shouldEqual 8
    List(List(), List(15), List(12), List(12, 15), List(9), List(9, 15), List(9, 12), List(9, 12, 15)).foreach(y should contain (_))

    val z = findSubsets(Array(2, 4, 6, 8))
    z.size shouldEqual 16

    val a = findSubsets(Array(2, 4, 6, 8, 10))
    a.size shouldEqual 32
  }

  "Staircase algorithm" should "pass all tests" in {
    // 0
    staircase(0) shouldEqual 0
    // 1
    staircase(1) shouldEqual 1
    // 11, 2
    staircase(2) shouldEqual 2
    // 111, 12, 21, 3
    staircase(3) shouldEqual 4
    // 1111, 13, 112, 211, 121, 22, 31
    staircase(4) shouldEqual 7
    staircase(5) shouldEqual 13
  }

  "Last Index algorithm" should "pass all tests" in {
    //
    lastIndex(Array(1, 2, 5, 5, 1, 2, 5, 4), 5) shouldEqual 6
    lastIndex(Array(1, 2, 5, 5, 1, 2, 5, 4), 7) shouldEqual -1
    lastIndex(Array(1, 2, 5, 5, 1, 2, 5, 4, 9), 9) shouldEqual 8
    lastIndex(Array(9, 2, 5, 5, 1, 2, 5, 4, 6), 9) shouldEqual 0
  }

  "A custom mutable raw Tree" should "behave as expected" in {
    //
    val tree = new Tree("apple")
    tree.root.value shouldEqual Some("apple")
    tree.root.left shouldEqual None
    tree.root.right shouldEqual None
    tree.root.left = new Node(Some("banana"))
    tree.root.right = new Node(Some("cherry"))
    tree.root.left.get.value shouldEqual Some("banana")
    tree.root.right.get.value shouldEqual Some("cherry")
    tree.root.left.get.left = new Node(Some("dates"))
    tree.root.left.get.left.get.value shouldEqual Some("dates")
  }

  "Binary Tree DFS Pre-Order Traversal algorithm" should "behave as expected" in {
    //
    val tree = new Tree("A")
    tree.root.left = new Node(Some("B"))
    tree.root.right = new Node(Some("C"))
    tree.root.left.get.left = new Node(Some("D"))
    tree.root.left.get.right = new Node(Some("H"))
    dfsPreOrderTraversal(tree.root) shouldEqual Vector("A", "B", "D", "H", "C")
    tree.root.right.get.left = new Node(Some("S"))
    tree.root.right.get.left.get.right = new Node(Some("M"))
    dfsPreOrderTraversal(tree.root) shouldEqual Vector("A", "B", "D", "H", "C", "S", "M")
  }

  "Binary Tree DFS In-Order Traversal algorithm" should "behave as expected" in {
    //
    val tree = new Tree("A")
    tree.root.left = new Node(Some("B"))
    tree.root.right = new Node(Some("C"))
    tree.root.left.get.left = new Node(Some("D"))
    tree.root.left.get.right = new Node(Some("H"))
    dfsInOrderTraversal(tree.root) shouldEqual Vector("D", "B", "H", "A", "C")
    tree.root.right.get.left = new Node(Some("S"))
    tree.root.right.get.left.get.right = new Node(Some("M"))
    dfsInOrderTraversal(tree.root) shouldEqual Vector("D", "B", "H", "A", "S", "M", "C")
  }

  "Binary Tree DFS Post-Order Traversal algorithm" should "behave as expected" in {
    //
    val tree = new Tree("A")
    tree.root.left = new Node(Some("B"))
    tree.root.right = new Node(Some("C"))
    tree.root.left.get.left = new Node(Some("D"))
    tree.root.left.get.right = new Node(Some("H"))
    dfsPostOrderTraversal(tree.root) shouldEqual Vector("D", "H", "B", "C", "A")
    tree.root.right.get.left = new Node(Some("S"))
    tree.root.right.get.left.get.right = new Node(Some("M"))
    dfsPostOrderTraversal(tree.root) shouldEqual Vector("D", "H", "B", "M", "S", "C", "A")
  }

  "Binary Tree BFS Traversal algorithm" should "behave as expected" in {
    //
    val tree = new Tree("A")
    tree.root.left = new Node(Some("B"))
    tree.root.right = new Node(Some("C"))
    tree.root.left.get.left = new Node(Some("D"))
    tree.root.left.get.right = new Node(Some("H"))
    bfsTraversal(tree.root) shouldEqual Vector("A", "B", "C", "D", "H")
    tree.root.right.get.left = new Node(Some("S"))
    tree.root.right.get.left.get.right = new Node(Some("M"))
    bfsTraversal(tree.root) shouldEqual Vector("A", "B", "C", "D", "H", "S", "M")
  }

  "Binary Tree BFS Display algorithm" should "behave as expected" in {
    //
    val tree = new Tree("A")
    tree.root.left = new Node(Some("B"))
    tree.root.right = new Node(Some("C"))
    tree.root.left.get.left = new Node(Some("D"))
    tree.root.left.get.right = new Node(Some("H"))
    //println(display(tree.root))
    tree.root.right.get.left = new Node(Some("S"))
    tree.root.right.get.left.get.right = new Node(Some("M"))
    //println(display(tree.root))
  }

  "Binary Search Tree Insert algorithm" should "behave as expected" in {
    //
    val tree = new BinarySearchTree(15)
    tree.insert(10)
    tree.root.value.get shouldEqual 15
    tree.root.left.get.value.get shouldEqual 10
    tree.root.right shouldEqual None
    tree.insert(20)
    tree.root.right.get.value.get shouldEqual 20
    tree.insert(15)
    tree.root.value.get shouldEqual 15
    tree.root.left.get.value.get shouldEqual 10
    tree.root.right.get.value.get shouldEqual 20
    tree.insert(11)
    tree.insert(9)
    tree.root.value.get shouldEqual 15
    tree.root.left.get.value.get shouldEqual 10
    tree.root.right.get.value.get shouldEqual 20
    tree.root.left.get.left.get.value.get shouldEqual 9
    tree.root.left.get.right.get.value.get shouldEqual 11
    //println(tree.display)
  }

  "Binary Search Tree Search algorithm" should "behave as expected" in {
    //
    val tree = new BinarySearchTree(100)
    tree.insert(50)
    tree.insert(150)
    tree.insert(75)
    tree.insert(175)
    tree.search(100).isDefined shouldEqual true
    tree.search(50).isDefined shouldEqual true
    tree.search(150).isDefined shouldEqual true
    tree.search(75).isDefined shouldEqual true
    tree.search(175).isDefined shouldEqual true
    tree.search(0).isDefined shouldEqual false
    tree.search(200).isDefined shouldEqual false
    tree.search(76).isDefined shouldEqual false
  }

  "Binary Search Tree Deletion algorithm" should "behave as expected" in {
    //
    val tree = new BinarySearchTree(15)
    tree.insert(10)
    tree.insert(20)
    tree.delete(20)
    tree.root.value.get shouldEqual 15
    tree.root.left.get.value.get shouldEqual 10
    tree.root.right.get.value shouldEqual None
    //println(tree.display)
    tree.insert(20)
    tree.insert(8)
    tree.insert(12)
    tree.insert(18)
    tree.insert(25)
    //println(tree.display)
    tree.delete(20)
    //println(tree.display)
    tree.root.value.get shouldEqual 15
    tree.root.left.get.value.get shouldEqual 10
    tree.root.right.get.value.get shouldEqual 18
    tree.root.right.get.left.get.value shouldEqual None
    tree.root.right.get.right.get.value.get shouldEqual 25
    //println(tree.display)
    val last = new BinarySearchTree(15)
    last.insert(10)
    last.insert(20)
    last.insert(8)
    last.insert(12)
    last.insert(18)
    last.insert(25)
    last.insert(1)
    last.insert(9)
    last.insert(11)
    last.insert(14)
    last.insert(17)
    last.insert(19)
    last.insert(22)
    last.insert(29)
    //println(last.display)
    last.delete(25)
    last.insert(21)
    //println(last.display)
  }

  "Binary Tree Diameter algorithm" should "behave as expected" in {
    //
    val tree = new Tree(1)
    tree.root.left = new Node(Some(2))
    tree.root.right = new Node(Some(3))
    tree.root.left.get.left = new Node(Some(4))
    tree.root.left.get.right = new Node(Some(5))
    diameter(tree.root) shouldEqual 3
    val treeA = new Tree(1)
    treeA.root.left = new Node(Some(2))
    diameter(treeA.root) shouldEqual 1
    val treeB = new Tree(1)
    diameter(treeB.root) shouldEqual 0
  }

  "Path From Root To Node algorithm" should "behave as expected" in {
    //
    val tree = new Tree(1)
    tree.root.left = new Node(Some(2))
    tree.root.right = new Node(Some(3))
    tree.root.left.get.left = new Node(Some(4))
    tree.root.left.get.right = new Node(Some(5))
    pathFromRootToNode(tree.root, 1) shouldEqual Vector(1)
    pathFromRootToNode(tree.root, 2) shouldEqual Vector(1, 2)
    pathFromRootToNode(tree.root, 3) shouldEqual Vector(1, 3)
    pathFromRootToNode(tree.root, 4) shouldEqual Vector(1, 2, 4)
    pathFromRootToNode(tree.root, 5) shouldEqual Vector(1, 2, 5)
    pathFromRootToNode(tree.root, 6) shouldEqual Vector()
  }

  "Hash Function" should "behave as expected" in {
    //
    val map = new Map[String, Int](10)
    map.getBucketIndex("one") shouldEqual map.getBucketIndex("neo")
    map.getBucketIndex("Apple") shouldEqual 8
    map.getBucketIndex("Vanguard") shouldEqual 4
    map.getBucketIndex("Tesla") shouldEqual 5
    map.getBucketIndex("Google") shouldEqual 5
    map.getBucketIndex("Amazon") shouldEqual 4
    map.getBucketIndex("Microsoft") shouldEqual 0
    //println(map.getBucketIndex("one")) // 142450 -> 2
    //println(map.getBucketIndex("neo")) // 155806 -> 2
  }

  "Hash Map Put algorithm" should "behave as expected" in {
    //
    val map = new Map[String, Int](10)
    map.size shouldEqual 0
    map.put("Apple", 1)
    map.size shouldEqual 1
    //println(map.bucketArray)
    map.put("Apple", 2)
    map.size shouldEqual 1
    map.bucketArray(8).head shouldEqual ("Apple", 2)
    map.put("Vanguard", 2)
    map.size shouldEqual 2
    map.put("Tesla", 3)
    map.size shouldEqual 3
    map.put("Google", 4)
    map.size shouldEqual 4
    map.put("Google", 5)
    map.size shouldEqual 4
  }

  "Hash Map Get algorithm" should "behave as expected" in {
    //
    val map = new Map[String, Int](10)
    map.put("Apple", 1)
    map.put("Vanguard", 2)
    map.put("Tesla", 3)
    map.put("Google", 4)
    //println(map.bucketArray)
    map.get("Apple") shouldEqual Some(1)
    map.get("Vanguard") shouldEqual Some(2)
    map.get("Tesla") shouldEqual Some(3)
    map.get("Google") shouldEqual Some(4)
    map.get("Amazon") shouldEqual None
  }

  "Rehashing algorithm" should "behave as expected" in {
    //
    val map = new Map[String, Int](5)
    map.bucketArray.length shouldEqual 5
    map.put("one", 1)
    map.put("two", 2)
    map.put("three", 3)
    map.put("neo", 11)
    map.size shouldEqual 4
    map.bucketArray.length shouldEqual 10
    map.size shouldEqual 4
  }

  "Hash Map Delete algorithm" should "behave as expected" in {
    //
    val map = new Map[String, Int](10)
    map.put("Apple", 1)
    map.put("Vanguard", 2)
    map.put("Tesla", 3)
    map.put("Google", 4)
    map.get("Apple") shouldEqual Some(1)
    map.delete("Apple")
    map.get("Apple") shouldEqual None
    map.bucketArray(5).size shouldEqual 2
    map.delete("Tesla")
    map.get("Tesla") shouldEqual None
    map.get("Google") shouldEqual Some(4)
    map.bucketArray(5).size shouldEqual 1
  }

  "Staircase queue algorithm" should "pass all tests" in {
    // 0
    staircaseQueue(0) shouldEqual 0
    // 1
    staircaseQueue(1) shouldEqual 1
    // 11, 2
    staircaseQueue(2) shouldEqual 2
    // 111, 12, 21, 3
    staircaseQueue(3) shouldEqual 4
    // 1111, 13, 112, 211, 121, 22, 31
    staircaseQueue(4) shouldEqual 7
    staircaseQueue(5) shouldEqual 13
    staircaseQueue(6) shouldEqual 24
    staircaseQueue(20) shouldEqual 121415
  }

  "String Key Hash Table algorithm" should "pass all tests" in {
    //
    val table = new HashTable
    table calculateHashValue "UDACITY" shouldEqual 8568
    table lookup "UDACITY" shouldEqual -1
    table store "UDACITY"
    table lookup "UDACITY" shouldEqual 8568
    table lookup "UDACIOUS" shouldEqual -1
    table store "UDACIOUS"
    table lookup "UDACIOUS" shouldEqual 8568
  }

  "Pair Sum To Target algorithm" should "pass all tests" in {
    //
    pairSumToTarget(Array(1, 5, 9, 7), 8) shouldEqual List((0, 3))
    pairSumToTarget(Array(0, 1, 2, 3, -4), -4) shouldEqual List((0, 4))
    pairSumToTarget(Array(0, 1, 10, 3, -4), 6) shouldEqual List((2, 4))
    pairSumToTarget(Array(0, 1, 10, 3, -4), 2) shouldEqual Nil
    pairSumToTarget(Array(1, 2, 5, 6, 9, 7), 8) shouldEqual List((0, 5), (1, 3))
  }

  "Longest Subsequence algorithm" should "pass all tests" in {
    //
    longestSubSequence(Array(5, 4, 7, 10, 1, 3, 55, 2)) shouldEqual Vector(1, 2, 3, 4, 5)
    longestSubSequence(Array(5, 4, 9, 6, 11, 2, 3, 13, 1)) shouldEqual Vector(1, 2, 3, 4, 5, 6)
    longestSubSequence(Array()) shouldEqual Vector()
    longestSubSequence(Array(6)) shouldEqual Vector(6)
    longestSubSequence(Array(1, 2, 0, 1)) shouldEqual Vector(0, 1, 2)
    longestSubSequence(Array(1, 3, 5, 2, 4)) shouldEqual Vector(1, 2, 3, 4, 5)
    longestSubSequence(Array(3, 9, 5, 1, 22)) shouldEqual Vector(1)
    longestSubSequence(Array(4, 0, -4, -2, 2, 5, 2, 0, -8, -8, -8, -8, -1, 7, 4, 5, 5, -4, 6, 6, -3)) shouldEqual Vector(-4, -3, -2, -1, 0)
    longestSubSequence(Array(9, 1, 4, 7, 3, -1, 0, 5, 8, -1, 6)) shouldEqual Vector(3, 4, 5, 6, 7, 8, 9)
  }

  "Length of Longest Subsequence algorithm" should "pass all tests" in {
    //
    lengthSubSequence(Array(5, 4, 7, 10, 1, 3, 55, 2)) shouldEqual 5
    lengthSubSequence(Array(5, 4, 9, 6, 11, 2, 3, 13, 1)) shouldEqual 6
    lengthSubSequence(Array()) shouldEqual 0
    lengthSubSequence(Array(6)) shouldEqual 1
    lengthSubSequence(Array(1, 2, 0, 1)) shouldEqual 3
    lengthSubSequence(Array(1, 3, 5, 2, 4)) shouldEqual 5
    lengthSubSequence(Array(3, 9, 5, 1, 22)) shouldEqual 1
    lengthSubSequence(Array(4, 0, -4, -2, 2, 5, 2, 0, -8, -8, -8, -8, -1, 7, 4, 5, 5, -4, 6, 6, -3)) shouldEqual 5
    lengthSubSequence(Array(9, 1, 4, 7, 3, -1, 0, 5, 8, -1, 6)) shouldEqual 7
  }

  // LeetCode

  "Binary Search Algorithm" should "pass all tests" in {
    //
    binarySearchLC(Array(10), 10) shouldEqual 0
    binarySearchLC(Array(1, 2, 3, 4), 8) shouldEqual -1
    binarySearchLC(Array(1, 2, 3, 4), 1) shouldEqual 0
    binarySearchLC(Array(1, 2, 3, 4), 4) shouldEqual 3
    binarySearchLC(Array(1, 2, 3, 4), 3) shouldEqual 2
    binarySearchLC(Array(1, 2, 3, 4, 5), 7) shouldEqual -1
    binarySearchLC(Array(1, 2, 3, 4, 5), 1) shouldEqual 0
    binarySearchLC(Array(1, 2, 3, 4, 5), 5) shouldEqual 4
    binarySearchLC(Array(1, 2, 3, 4, 5), 3) shouldEqual 2
  }

  "Two Sum" should "pass all tests" in {
    //
    twoSum(Array(2, 7, 11, 15), 9) shouldEqual Array(0, 1)
    twoSum(Array(3, 2, 4), 6) shouldEqual Array(1, 2)
    twoSum(Array(3, 3), 6) shouldEqual Array(0, 1)
  }

  "Longest Substring Without Repeating Characters" should "pass all tests" in {
    // Brute force approach is always the easy default, but unacceptable
    longestSubString("abcabcbb") shouldEqual 3
    longestSubString("bbbbb") shouldEqual 1
    longestSubString("pwwkew") shouldEqual 3
    longestSubString("") shouldEqual 0
    // Interesting. An empty space in a string is a valid character with ASCII value = 32
    longestSubString(" ") shouldEqual 1
    longestSubString("dvdf") shouldEqual 3
    longestSubString("davdf") shouldEqual 4
    longestSubString("tmmzuxt") shouldEqual 5
    longestSubString("bbtablud") shouldEqual 6
  }

  "Depth First Search" should "pass all tests" in {
    // root = [3,9,20,null,null,15,7]
    val tree = new TreeNode(3)
    tree.left = new TreeNode(9)
    tree.right = new TreeNode(20)
    tree.right.left = new TreeNode(15)
    tree.right.right = new TreeNode(7)
    depthFirstSearch(tree) shouldEqual List(3, 9, 20, 15, 7)
    val treeB = new TreeNode(3)
    treeB.left = new TreeNode(9)
    treeB.right = new TreeNode(20)
    treeB.left.left = new TreeNode(15)
    treeB.left.right = new TreeNode(7)
    depthFirstSearch(treeB) shouldEqual List(3, 9, 15, 7, 20)
  }

  "Breadth First Search" should "pass all tests" in {
    // root = [3,9,20,null,null,15,7]
    val tree = new TreeNode(3)
    tree.left = new TreeNode(9)
    tree.right = new TreeNode(20)
    tree.right.left = new TreeNode(15)
    tree.right.right = new TreeNode(7)
    breadthFirstSearch(tree) shouldEqual Vector(3, 9, 20, 15, 7)
    val treeB = new TreeNode(3)
    treeB.left = new TreeNode(9)
    treeB.right = new TreeNode(20)
    treeB.left.left = new TreeNode(15)
    treeB.left.right = new TreeNode(7)
    breadthFirstSearch(treeB) shouldEqual Vector(3, 9, 20, 15, 7)
  }

  "Maximum Depth of Binary Tree" should "pass all tests" in {
    //
    val tree = new TreeNode(3)
    tree.left = new TreeNode(9)
    tree.right = new TreeNode(20)
    tree.right.left = new TreeNode(15)
    tree.right.right = new TreeNode(7)
    maxDepth(tree) shouldEqual 3
    val treeA = new TreeNode(1)
    treeA.right = new TreeNode(2)
    maxDepth(treeA) shouldEqual 2
    val treeB = new TreeNode(3)
    treeB.left = new TreeNode(9)
    treeB.right = new TreeNode(20)
    treeB.left.left = new TreeNode(15)
    treeB.left.right = new TreeNode(7)
    maxDepth(treeB) shouldEqual 3
  }

  "String to Integer (atoi)" should "pass all tests" in {
    //
    myAtoi("42") shouldEqual 42
    myAtoi("   -42") shouldEqual -42
    myAtoi("4193 with words") shouldEqual 4193
    myAtoi("  ") shouldEqual 0
    myAtoi(" 00493  ") shouldEqual 493
    myAtoi(" 004-93  ") shouldEqual 4 // 4 did not equal -4
    myAtoi(" 00409+  ") shouldEqual 409
    myAtoi("  -") shouldEqual 0
    myAtoi("  -.79") shouldEqual 0
    myAtoi(" 031.+37") shouldEqual 31
    myAtoi(" 037-.") shouldEqual 37 // 37 did not equal -37
    myAtoi("-91283472332") shouldEqual scala.math.pow(-2, 31).toInt
    myAtoi("91283472332") shouldEqual (scala.math.pow(2, 31) - 1).toInt
    myAtoi("+-12") shouldEqual 0
    myAtoi("00000-42a1234") shouldEqual 0
  }

  "Integer to Roman" should "pass all tests" in {
    integerToRoman(3) shouldEqual "III"
    integerToRoman(4) shouldEqual "IV"
    integerToRoman(99) shouldEqual "XCIX"
    integerToRoman(58) shouldEqual "LVIII"
    integerToRoman(1994) shouldEqual "MCMXCIV"
  }

  "Roman to Integer" should "pass all tests" in {
    romanToInteger("III") shouldEqual 3
    romanToInteger("IV") shouldEqual 4
    romanToInteger("XCIX") shouldEqual 99
    romanToInteger("LVIII") shouldEqual 58
    romanToInteger("MCMXCIV") shouldEqual 1994
  }

  "Three Sum" should "pass all tests" in {
    // Input always contains 3+ elements
    // -4, -1, 0, 1, 2
    threeSum(Array(-1, 0, 1, 2, -1, -4)) should contain allOf (List(-1, 0, 1), List(-1, -1, 2))
    threeSum(Array(-1, -1, -1, 3)) shouldEqual List()
    threeSum(Array(1, 2, 3)) shouldEqual List()
    threeSum(Array(0, 0, 0)) shouldEqual List(List(0, 0, 0))
  }

  "Three Sum Closest" should "pass all tests" in {
    //
    threeSumClosest(Array(2, 2, 2), 6) shouldEqual 6
    threeSumClosest(Array(2, 2, 1, 1), 6) shouldEqual 5
    threeSumClosest(Array(2, 2, -1, -1, 3), 6) shouldEqual 7
    threeSumClosest(Array(-1, 2, 1, -4), 1) shouldEqual 2
    threeSumClosest(Array(0, 0, 0), 1) shouldEqual 0
    threeSumClosest(Array(4, 0, 5, -5, 3, 3, 0, -4, -5), -2) shouldEqual -2
    // This input would not come up
    //threeSumClosest(Array(2, 2, 1, 1, 3), 6) shouldEqual ???
  }

  "Four Sum" should "pass all tests" in {
    //
    val testA = fourSum(Array(1, 0, -1, 0, -2, 2), 0)
    testA should contain allOf (List(-1, 0, 0, 1), List(-2, 0, 0, 2), List(-2, -1, 1, 2))
    testA.size shouldEqual 3
    val testB = fourSum(Array(2, 2, 2, 2, 2), 8)
    testB shouldEqual List(List(2, 2, 2, 2))
    testB.size shouldEqual 1
    val testC = fourSum(Array(1000000000, 1000000000, 1000000000, 1000000000), -294967296)
    testC shouldEqual List()
    testC.size shouldEqual 0
  }

  "Group Anagrams" should "pass all tests" in {
    val testA = groupAnagrams(Array("eat", "tea", "tan", "ate", "nat", "bat"))
    testA.size shouldEqual 3
    testA should contain allOf (List("bat"), List("tan", "nat"), List("eat", "tea", "ate"))
    val testB = groupAnagrams(Array("bat", "nat", "tea"))
    testB.size shouldEqual 3
    testB should contain allOf(List("bat"), List("nat"), List("tea"))
    val testC = groupAnagrams(Array("tea"))
    testC.size shouldEqual 1
    testC shouldEqual List(List("tea"))
    val testD = groupAnagrams(Array("", "", ""))
    testD.size shouldEqual 1
    testD shouldEqual List(List("", "", ""))
    val testE = groupAnagrams(Array("", "tea", "eat"))
    testE.size shouldEqual 2
    testE shouldEqual List(List("tea", "eat"), List(""))
    val testF = groupAnagrams(Array("tea", "tea"))
    testF.size shouldEqual 1
    testF shouldEqual List(List("tea", "tea"))
    val testH = groupAnagrams(Array("eat", "tea", "tea"))
    testH.size shouldEqual 1
    testH shouldEqual List(List("eat", "tea", "tea"))
    val testG = groupAnagrams(Array("aab", "baa"))
    testG.size shouldEqual 1
    testG shouldEqual List(List("aab", "baa"))
  }

  "Spiral Matrix" should "pass all tests" in {
    // m x n (square matrix)
    spiralMatrix(Array(Array())) shouldEqual List()
    spiralMatrix(Array(Array(3, 2, 1))) shouldEqual List(3, 2, 1)
    //spiralMatrix(Array(Array(3, 2, 1), Array())) shouldEqual List(3, 2, 1)
    //spiralMatrix(Array(Array(), Array(2, 4, 6), Array())) shouldEqual List(6, 2, 4)
    //spiralMatrix(Array(Array(1), Array(2, 4, 6), Array(7))) shouldEqual List(1, 6, 7, 2, 4)
    spiralMatrix(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))) shouldEqual List(1, 2, 3, 6, 9, 8, 7, 4, 5)
    spiralMatrix(Array(Array(1,2,3,4), Array(5,6,7,8), Array(9,10,11,12))) shouldEqual List(1,2,3,4,8,12,11,10,9,5,6,7)
    //Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)).tail.transpose.foreach(c => c.foreach(println))
  }

  "Minimum Window Substring" should "pass all tests" in {
    //
    minimumWindowSubstring("wahienljlpoxyz", "hello") shouldEqual "hienljlpo"
    minimumWindowSubstring("ADOBECODEBANC", "ABC") shouldEqual "BANC"
    minimumWindowSubstring("ADOBEACODEBANC", "ABC") shouldEqual "BEAC"
    minimumWindowSubstring("a", "a") shouldEqual "a"
    minimumWindowSubstring("a", "aa") shouldEqual ""
    minimumWindowSubstring("aa", "bb") shouldEqual ""
    minimumWindowSubstring("ADOBEC", "ABC") shouldEqual "ADOBEC"
  }

  "Valid Palindrome" should "pass all tests" in {
    //
    validPalindrome("A man, a plan, a canal: Panama") shouldEqual true
    validPalindrome("race a car") shouldEqual false
    // s is an empty string "" after removing non-alphanumeric characters.
    // Since an empty string reads the same forward and backward, it is a palindrome.
    validPalindrome(" ") shouldEqual true
  }

  "Majority Element II" should "pass all tests" in {
    //
    majorityElementII(Array()) shouldEqual List()
    majorityElementII(Array(1)) shouldEqual List(1)
    majorityElementII(Array(2, 2)) shouldEqual List(2)
    majorityElementII(Array(1, 2)) shouldEqual List(1, 2)
    majorityElementII(Array(3, 2, 3)) shouldEqual List(3)
    majorityElementII(Array(1, 2, 3, 4, 5, 6)) shouldEqual List()
    majorityElementII(Array(1, 1, 1, 1, 5, 6)) shouldEqual List(1)
    majorityElementII(Array(1, 2, 3, 4, 5)) shouldEqual List()
    majorityElementII(Array(1, 2, 3, 3, 5, 6)) shouldEqual List()
    majorityElementII(Array(2, 2, 1, 3)) shouldEqual List(2)
    majorityElementII(Array(1, 3, 3, 4)) shouldEqual List(3)
    majorityElementII(Array(0, 3, 4, 3)) shouldEqual List(3)
    majorityElementII(Array(0, 3, 4, 0)) shouldEqual List(0)
    majorityElementII(Array(0, 0, 0)) shouldEqual List(0)
    // Amazing. Elements default to 0 instead of null
    // Array.ofDim[Int](4).foreach(println)
  }

  "Product of Array Except Self" should "pass all tests" in {
    //
    arrayProduct(Array()) shouldEqual Array()
    arrayProduct(Array(0)) shouldEqual Array()
    arrayProduct(Array(1, 1, 2, 3)) shouldEqual Array(6, 6, 3, 2)
    arrayProduct(Array(1, 2, 3, 4)) shouldEqual Array(24, 12, 8, 6)
    arrayProduct(Array(-1, 1, 0, -3, 3)) shouldEqual Array(0, 0, 9, 0, 0)
    arrayProduct(Array(-1, 1, 0, -3, 3, 0)) shouldEqual Array(0, 0, 0, 0, 0, 0)
    arrayProduct(Array(1, -1)) shouldEqual Array(-1, 1)
  }

  "Missing Number" should "pass all tests" in {
    //
    missingNumber(Array(3, 0, 1)) shouldEqual 2
    missingNumber(Array(0 , 1)) shouldEqual 2
    missingNumber(Array(9, 6, 4, 2, 3, 5, 7, 0, 1)) shouldEqual 8
    missingNumber(Array(1 , 0)) shouldEqual 2
  }

  "First Unique Character in a String" should "pass all tests" in {
    //
    firstUniqueChar("leetcode") shouldEqual 0
    firstUniqueChar("loveleetcode") shouldEqual 2
    firstUniqueChar("aabb") shouldEqual -1
    firstUniqueChar("") shouldEqual -1
    firstUniqueChar("dddccdbba") shouldEqual 8
  }

  "Subarray Sum Equals K" should "pass all tests" in {
    //
    subarraySumK(Array(1, 1, 1), 2) shouldEqual 2
    subarraySumK(Array(1, 2, 3), 3) shouldEqual 2
    subarraySumK(Array(1, 1, 1), 1) shouldEqual 3
    subarraySumK(Array(1, 2, 1), 5) shouldEqual 0
    subarraySumK(Array(6, 9, 4, 1, 2, 3), 3) shouldEqual 2
  }

  "Squares of a Sorted Array" should "pass all tests" in {
    //
    squaresSortedArray(Array()) shouldEqual Array()
    squaresSortedArray(Array(5)) shouldEqual Array(25)
    squaresSortedArray(Array(0 , 0)) shouldEqual Array(0, 0)
    squaresSortedArray(Array(-2, 0, 0)) shouldEqual Array(0, 0, 4)
    squaresSortedArray(Array(-3, -2, -1, 1, 2, 3)) shouldEqual Array(1, 1, 4, 4, 9, 9)
    squaresSortedArray(Array(-3, -2, -1, 1)) shouldEqual Array(1, 1, 4, 9)
    squaresSortedArray(Array(-3, -2, -1, 0, 1, 2, 3)) shouldEqual Array(0, 1, 1, 4, 4, 9, 9)
    squaresSortedArray(Array(0, 1, 2, 3)) shouldEqual Array(0, 1, 4, 9)
    squaresSortedArray(Array(-3, -2, -1, 0)) shouldEqual Array(0, 1, 4, 9)
  }

  "Valid Parentheses" should "pass all tests" in {
    //
    validParentheses("") shouldEqual true
    validParentheses("()") shouldEqual true
    validParentheses("{{") shouldEqual false
    validParentheses("}{") shouldEqual false
    validParentheses("]") shouldEqual false
    validParentheses("{[]}") shouldEqual true
    validParentheses("{[)}") shouldEqual false
    validParentheses("{}[)}") shouldEqual false
    validParentheses("({[]}{{}})") shouldEqual true
    validParentheses("{[]{{{}}}}") shouldEqual true
  }

  "Trapping Rain Water" should "pass all tests" in {
    //
    trappingRainWater(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) shouldEqual 6
    trappingRainWater(Array(4, 2, 0, 3, 2, 5)) shouldEqual 9
  }

  "Sparse Matrix Multiplication" should "pass all tests" in {
    //
    sparseMatrixMultiplication(Array(Array(0)), Array(Array(0))) shouldEqual Array(Array(0))
    sparseMatrixMultiplication(Array(Array(1,0,0), Array(-1,0,3)), Array(Array(7,0,0), Array(0,0,0), Array(0,0,1))) shouldEqual Array(Array(7,0,0), Array(-7,0,3))
    sparseMatrixMultiplication(Array(Array(1, -5)), Array(Array(12), Array(-1))) shouldEqual Array(Array(17))
  }

  "Add Two Numbers" should "pass all tests" in {
    //
    var nodeA = new ListNode(2)
    nodeA.next = new ListNode(4)
    nodeA.next.next = new ListNode(3)
    var nodeB = new ListNode(5)
    nodeB.next = new ListNode(6)
    nodeB.next.next = new ListNode(4)
    var result = addTwoNumbers(nodeA, nodeB)
    result.x shouldEqual 7
    result.next.x shouldEqual 0
    result.next.next.x shouldEqual 8
    // 342 + 465 = 807

    nodeA = new ListNode(0)
    nodeB = new ListNode(0)
    result = addTwoNumbers(nodeA, nodeB)
    result.x shouldEqual 0

    nodeA = new ListNode(9)
    nodeA.next = new ListNode(9)
    nodeA.next.next = new ListNode(9)
    nodeA.next.next.next = new ListNode(9)
    nodeA.next.next.next.next = new ListNode(9)
    nodeA.next.next.next.next.next = new ListNode(9)
    nodeA.next.next.next.next.next.next = new ListNode(9)
    nodeB = new ListNode(9)
    nodeB.next = new ListNode(9)
    nodeB.next.next = new ListNode(9)
    nodeB.next.next.next = new ListNode(9)
    result = addTwoNumbers(nodeA, nodeB)
    result.x shouldEqual 8
    result.next.x shouldEqual 9
    result.next.next.x shouldEqual 9
    result.next.next.next.x shouldEqual 9
    result.next.next.next.next.x shouldEqual 0
    result.next.next.next.next.next.x shouldEqual 0
    result.next.next.next.next.next.next.x shouldEqual 0
    result.next.next.next.next.next.next.next.x shouldEqual 1
    // sum = 10,009,998
  }

  "Merge Two Sorted Lists" should "pass all tests" in {
    //
    var nodeA = new ListNode(1)
    nodeA.next = new ListNode(2)
    nodeA.next.next = new ListNode(4)
    var nodeB = new ListNode(1)
    nodeB.next = new ListNode(3)
    nodeB.next.next = new ListNode(4)
    var result = mergeSortedLists(nodeA, nodeB)
    result.x shouldEqual 1
    result.next.x shouldEqual 1
    result.next.next.x shouldEqual 2
    result.next.next.next.x shouldEqual 3
    result.next.next.next.next.x shouldEqual 4
    result.next.next.next.next.next.x shouldEqual 4

    //nodeA = new ListNode(null)
    //nodeB = new ListNode(null)
    result = mergeSortedLists(null, null)
    result should be (null)

    //nodeA = new ListNode(null)
    nodeB = new ListNode()
    result = mergeSortedLists(null, nodeB)
    result.x shouldEqual 0
  }

  "Reverse Linked List" should "pass all tests" in {
    //
    var nodeA = new ListNode(1)
    nodeA.next = new ListNode(2)
    nodeA.next.next = new ListNode(3)
    nodeA.next.next.next = new ListNode(4)
    nodeA.next.next.next.next = new ListNode(5)
    var result = reverseLinkedList(nodeA)
    result.x shouldEqual 5
    result.next.x shouldEqual 4
    result.next.next.x shouldEqual 3
    result.next.next.next.x shouldEqual 2
    result.next.next.next.next.x shouldEqual 1

    nodeA = new ListNode(1)
    result = reverseLinkedList(nodeA)
    result.x shouldEqual 1

    result = reverseLinkedList(null)
    result should be (null)
  }

  "Same Tree" should "pass all tests" in {
    // Commit code
    var nodeA = new TreeNode(1)
    nodeA.left = new TreeNode(2)
    nodeA.right = new TreeNode(3)
    var nodeB = new TreeNode(1)
    nodeB.left = new TreeNode(2)
    nodeB.right = new TreeNode(3)
    sameTree(nodeA, nodeB) shouldEqual true

    nodeA = new TreeNode(1)
    nodeA.left = new TreeNode(2)
    nodeA.right = new TreeNode(3)
    nodeB = new TreeNode(1)
    nodeB.left = new TreeNode(3)
    nodeB.right = new TreeNode(2)
    sameTree(nodeA, nodeB) shouldEqual false

    nodeB = new TreeNode(1)
    nodeB.left = new TreeNode(3)
    nodeB.right = new TreeNode(2)
    sameTree(null, nodeB) shouldEqual false

    sameTree(null, null) shouldEqual true
  }

  "Clone Graph" should "pass all tests" in {
    // Commit code
    var nodeA = new LeetCodeNode(1)
    var nodeB = new LeetCodeNode(2)
    var nodeC = new LeetCodeNode(3)
    var nodeD = new LeetCodeNode(4)
    nodeB.neighbors = List(nodeA, nodeC)
    nodeD.neighbors = List(nodeA, nodeC)
    nodeC.neighbors = List(nodeB, nodeD)
    nodeA.neighbors = List(nodeB, nodeD)
    var newNode = cloneGraph(nodeA)
    newNode.value shouldEqual 1
    newNode.neighbors.size shouldEqual 2
    newNode.neighbors.head.value shouldEqual 2
    newNode.neighbors.last.value shouldEqual 4
  }

  "" should "pass all tests" in {
    // Commit code
  }
}

