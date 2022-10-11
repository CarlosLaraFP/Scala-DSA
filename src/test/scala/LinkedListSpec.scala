import com.valinor.lists.LinkedListAlgorithms._
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

object LinkedListSpec extends AnyFlatSpec with Matchers {
  /*
    ANY SCALA CODING INTERVIEW REQUIRES THIS MINIMUM [CUSTOM] SETUP (NO LEETCODE)
  */
  def addOneTests(): Unit = {
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

  def duplicateNumberTests(): Unit = {
    //
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

  def maxSumSubArrayTests(): Unit = {
    //
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

  def nthRowPascalTests(): Unit = {
    nthRowPascal(0) shouldEqual List(1)
    nthRowPascal(1) shouldEqual List(1, 1)
    nthRowPascal(2) shouldEqual List(1, 2, 1)
    nthRowPascal(3) shouldEqual List(1, 3, 3, 1)
    nthRowPascal(4) shouldEqual List(1, 4, 6, 4, 1)
    nthRowPascal(5) shouldEqual List(1, 5, 10, 10, 5, 1)
  }

  def oddEvenNodesTests(): Unit = {
    oddEvenNodes(List(1, 2, 3, 4, 5)) shouldEqual List(1, 3, 5, 2, 4)
    oddEvenNodes(List(1, 2, 3, 4, 5, 6)) shouldEqual List(1, 3, 5, 2, 4, 6)
    oddEvenNodes(List(2, 1, 3, 5, 6, 4, 7)) shouldEqual List(2, 3, 6, 7, 1, 5, 4)
    oddEvenNodes(List[Int]()) shouldEqual List[Int]()
    oddEvenNodes(List[Int](2)) shouldEqual List[Int](2)
    oddEvenNodes(List(5, 5, 4, 4, 1)) shouldEqual List(5, 4, 1, 5, 4)
  }

  def skipIDeleteJTests(): Unit = {
    skipIDeleteJ(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 2, 3) shouldEqual List(1, 2, 6, 7, 11, 12)
    skipIDeleteJ(List[Int](), 8, 9) shouldEqual List[Int]()
    skipIDeleteJ(List(1, 3, 5, 7, 9), 1, 1) shouldEqual List(1, 5, 9)
    skipIDeleteJ(List(1, 3, 5, 7, 9), 1, 0) shouldEqual List(1, 3, 5, 7, 9)
    skipIDeleteJ(List(1, 3, 5, 7), 0, 1) shouldEqual List[Int]()
    skipIDeleteJ(List(1, 3, 5, 7, 11, 13), 0, 2) shouldEqual List[Int]()
    skipIDeleteJ(List(1, 3, 5, 7, 11, 13, 19), 0, 20) shouldEqual List[Int]()
    skipIDeleteJ(List(1, 3, 5, 7, 11, 13, 19), 1, 5) shouldEqual List(1, 19)
  }

  def swapNodesTests(): Unit = {
    swapNodes(List(3, 4, 5, 2, 6, 1, 9), 2, 5) shouldEqual List(3, 4, 1, 2, 6, 5, 9)
    swapNodes(List(3, 4, 5, 2, 6, 1, 9), 0, 0) shouldEqual List(3, 4, 5, 2, 6, 1, 9)
    swapNodes(List(3, 4, 5, 2, 6, 1, 9), 3, 3) shouldEqual List(3, 4, 5, 2, 6, 1, 9)
    swapNodes(List(3, 4, 5, 2, 6, 1, 9), 10, 16) shouldEqual List(3, 4, 5, 2, 6, 1, 9)
    swapNodes(List(3, 4, 5, 2, 6, 1, 9), 0, 10) shouldEqual List(9, 4, 5, 2, 6, 1, 3)
    swapNodes(List(9, 8, 7, 6), 1, 3) shouldEqual List(9, 6, 7, 8)
    swapNodes(List(9, 8, 7, 6), 1, 4) shouldEqual List(9, 6, 7, 8)
  }

  def main(args: Array[String]): Unit = {
    println("Running unit tests...")
    this.addOneTests()
    this.duplicateNumberTests()
    this.maxSumSubArrayTests()
    this.nthRowPascalTests()
    this.oddEvenNodesTests()
    this.skipIDeleteJTests()
    this.swapNodesTests()
    println("All unit tests passed!")
  }
}
