package queues
import java.util
import org.scalatest
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

class DoublyLinkedNode[T](newval: T) extends visualization.Visualizable {
  protected var contents: T = newval
  protected var n: DoublyLinkedNode[T] = _
  protected var p: DoublyLinkedNode[T] = _
  
  def set(newval: T) = { contents = newval }
  def data = contents
  def next = n 
  def prev = p
  def setnext(newnext: DoublyLinkedNode[T]) = { n = newnext }
  def setprev(newprev: DoublyLinkedNode[T]) = { p = newprev }
  
  def snip() {
    if (n == null) {
      if (p == null) {
        null
      } else {
        p.setnext(null)
        p
      }
    } else {
      if (p == null) {
        n.setprev(null)
        n
      } else {
        p.setnext(n)
        n.setprev(p)
        p
      }
    }
  }
}

class DoublyLinkedList[T] extends visualization.Visualizable {
  protected var head: DoublyLinkedNode[T] = _
  protected var tail: DoublyLinkedNode[T] = _
  
  def gethead = head
  def gettail = tail
  
  def insertAtHead(newval: T) {
    var newnode = new DoublyLinkedNode[T](newval)
    newnode.setnext(head)  
    if (head == null) {
      head = newnode
      tail = newnode
    } else {
      head.setprev(newnode)
      head = newnode
    }
  }
  
  def insertAtTail(newval: T) {
	var newnode = new DoublyLinkedNode[T](newval)
    newnode.setprev(tail)
    if (tail == null) {
      head = newnode
      tail = newnode
    } else {
      tail.setnext(newnode)
      tail = newnode
    }
  }
  
  def removeNode(oldnode: DoublyLinkedNode[T]) {
    if (head == oldnode) head = oldnode.next
    if (tail == oldnode) tail = oldnode.prev
    oldnode.snip()
  }
  
  def toArray(implicit m: ClassManifest[T]): Array[T] = {
    var returned = Array[T]()
    var thisnode = head
    while (thisnode != null) {
      returned :+= thisnode.data
      thisnode = thisnode.next
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
    newnode.data should equal (teststring)
  }
  
  "A doubly linked list" should "store a node with a value of type string." in {
    val newlist = new DoublyLinkedList[String]
    newlist.insertAtHead(teststring)
    newlist.gethead.data should equal (teststring)
    newlist.gettail.data should equal (teststring)
    newlist.gethead should equal (newlist.gettail)
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
    
    newlist.removeNode(newlist.gethead)
    newlist.removeNode(newlist.gettail)
    newlist.removeNode(newlist.gethead.next)
    
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
    newlist.gethead should equal (newlist.gettail)
    newlist.removeNode(newlist.gethead)
    newlist.gethead should equal (null)
    newlist.gettail should equal (null)
    
    newlist.insertAtTail(1)
    newlist.gethead should equal (newlist.gettail)
    newlist.removeNode(newlist.gettail)
    newlist.gethead should equal (null)
    newlist.gettail should equal (null)
  }
}