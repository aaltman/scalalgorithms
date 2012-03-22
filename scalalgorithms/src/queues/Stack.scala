package queues
import org.scalatest
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

class Stack[T] extends DoublyLinkedList[T] {
  def push(newval: T) {
    insertAtHead(newval)
  }
  
  def pop = {
    val returned = head.data
    removeNode(head)
    returned
  }
}

@RunWith(classOf[JUnitRunner])
class StackTest extends FlatSpec with ShouldMatchers {
  val testvalues = Array(1,2,3,4,5)
  
  "A stack" should "push and pop in reverse order" in {
    val newstack = new Stack[Int]
    for (newval <- testvalues) {
      newstack.push(newval)
    }
    
    for (i <- 0 until testvalues.length) {
      newstack.pop should equal (testvalues(testvalues.length - 1 - i)) 
    }
  }
}
