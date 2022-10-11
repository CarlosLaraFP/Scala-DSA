import com.valinor.lists.{DoublyLinkedList, NestedSortedLinkedList, SortedLinkedList}
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

object DoublyLinkedListSpec extends AnyFlatSpec with Matchers {
  /*
    Using Scalatest for systematic unit testing.
  */
  def testDoublyLinkedList(): Unit = {
    /*
    */
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

  def testSortedLinkedList(): Unit = {
    /*
    */
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

  def testNestedLinkedList(): Unit = {
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

  def main(args: Array[String]): Unit = {
    println("Running unit tests...")
    this.testDoublyLinkedList()
    this.testSortedLinkedList()
    this.testNestedLinkedList()
    println("All unit tests passed!")
  }
}
