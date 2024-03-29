package queues
import java.util
import org.scalatest
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

class DoublyLinkedNode[T](newval: T) extends visualization.Visualizable {
  var contents: T = newval
  var n: DoublyLinkedNode[T] = _
  var p: DoublyLinkedNode[T] = _

  def snip() {
    if (n == null) {
      if (p == null) {
        null
      } else {
        p.n = null
        p
      }
    } else {
      if (p == null) {
        n.p = null
        n
      } else {
        p.n = n
        n.p = p
        p
      }
    }
  }
}

class DoublyLinkedList[T] extends visualization.Visualizable {
  var head: DoublyLinkedNode[T] = _
  var tail: DoublyLinkedNode[T] = _
  
  def insertAtHead(newval: T) {
    var newnode = new DoublyLinkedNode[T](newval)
    newnode.n = head
    if (head == null) {
      head = newnode
      tail = newnode
    } else {
      head.p = newnode
      head = newnode
    }
  }
  
  def insertAtTail(newval: T) {
	var newnode = new DoublyLinkedNode[T](newval)
    newnode.p = tail
    if (tail == null) {
      head = newnode
      tail = newnode
    } else {
      tail.n = newnode
      tail = newnode
    }
  }
  
  def removeNode(oldnode: DoublyLinkedNode[T]) {
    if (head == oldnode) head = oldnode.n
    if (tail == oldnode) tail = oldnode.p
    oldnode.snip()
  }
  
  def toArray(implicit m: ClassManifest[T]): Array[T] = {
    var returned = Array[T]()
    var thisnode = head
    while (thisnode != null) {
      returned :+= thisnode.contents
      thisnode = thisnode.n
    }
    returned
  }
}

@RunWith(classOf[JUnitRunner])
class DLLTest extends FlatSpec with ShouldMatchers {
  protected val teststring = "Test string for storing in doubly linked list."
  protected val testnums = Array(1,2,3,4,5)
  
  "A doubly linked node" should "store a value of type string." in {
    val newnode = new DoublyLinkedNode(teststring)
    newnode.contents should equal (teststring)
  }
  
  "A doubly linked list" should "store a node with a value of type string." in {
    val newlist = new DoublyLinkedList[String]
    newlist.insertAtHead(teststring)
    newlist.head.contents should equal (teststring)
    newlist.tail.contents should equal (teststring)
    newlist.head should equal (newlist.tail)
  }
  
  "A doubly linked list" should "preserve the order when inserting at the tail." in {
    val newlist = new DoublyLinkedList[Int]
    for (newval <- testnums) {
      newlist.insertAtTail(newval)
    }
    val newarray = newlist.toArray
    newarray.length should equal (testnums.length)
    
    for (i <- 0 until newarray.length) {
      newarray(i) should equal (testnums(i))
    }
  }
  
  "A doubly linked list" should "reverse the order when inserting at the head." in {
    val newlist = new DoublyLinkedList[Int]
    for (newval <- testnums) {
      newlist.insertAtHead(newval)
    }
    val newarray = newlist.toArray
    newarray.length should equal (testnums.length)
    
    for (i <- 0 until newarray.length) {
      newarray(i) should equal (testnums(testnums.length - 1 - i))
    }
  }
  
  "A doubly linked list" should "support snipping the head, tail or a middle element." in {
    val newlist = new DoublyLinkedList[Int]
    for (newval <- testnums) {
      newlist.insertAtTail(newval)
    }
    
    newlist.removeNode(newlist.head)
    newlist.removeNode(newlist.tail)
    newlist.removeNode(newlist.head.n)
    
    val cmp = Array(2,4)
    val newarray = newlist.toArray
    newarray.length should equal (cmp.length)
    
    for (i <- 0 until newarray.length) {
      newarray(i) should equal (cmp(i))
    }    
  }
  
  "A single element doubly linked list" should "support removing from head or tail." in {
    val newlist = new DoublyLinkedList[Int]
    
    newlist.insertAtHead(1)
    newlist.head should equal (newlist.tail)
    newlist.removeNode(newlist.head)
    newlist.head should equal (null)
    newlist.tail should equal (null)
    
    newlist.insertAtTail(1)
    newlist.head should equal (newlist.tail)
    newlist.removeNode(newlist.tail)
    newlist.head should equal (null)
    newlist.tail should equal (null)
  }
}