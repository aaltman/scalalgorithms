package queues

class Queue[T] {
  protected var elementList: DoublyLinkedList[T] = _
  protected var length = 0
  
  def enqueue(newval: T) {
    elementList.insertAtTail(newval)
    length += 1
  }
  
  def dequeue: T = {
    if (length == 0) {
      throw new java.util.NoSuchElementException
    } 
    var oldval = elementList.head.contents
    elementList.removeNode(elementList.head)
    oldval
  }
}