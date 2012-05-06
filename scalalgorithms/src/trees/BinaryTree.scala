package trees
import scala.collection

abstract class BinaryTree[T](stored_value: T) { 
  var contents = stored_value
  var l: this.type = _
  var r: this.type = _
  
  def weight: Int = {
    if (l == null && r == null) {
      0
    } else if (l == null) {
      1 + r.weight
    } else if (r == null) {
      -1 + l.weight
    } else {
      r.weight + l.weight
    }
  }
  
  def preorderTraversalAsArray(implicit m: ClassManifest[T]): Array[T] = {
    if (l == null && r == null) {
      Array[T](contents)
    } else if (l == null) {
      contents +: r.preorderTraversalAsArray
    } else if (r == null) {
      contents +: l.preorderTraversalAsArray
    } else {
      contents +: (l.preorderTraversalAsArray ++ r.preorderTraversalAsArray)
    }
  }
  
  def inorderTraversalAsArray(implicit m: ClassManifest[T]): Array[T] = {
    if (l == null && r == null) {
      Array[T](contents)
    } else if (l == null) {
      contents +: r.inorderTraversalAsArray
    } else if (r == null) {
      l.inorderTraversalAsArray :+ contents
    } else {
      (l.inorderTraversalAsArray :+ contents) ++ r.inorderTraversalAsArray
    }
  }
  
  def postorderTraversalAsArray(implicit m: ClassManifest[T]): Array[T] = {
    if (l == null && r == null) {
      Array[T](contents)
    } else if (l == null) {
      r.postorderTraversalAsArray :+ contents
    } else if (r == null) {
      l.postorderTraversalAsArray :+ contents
    } else {
      (l.postorderTraversalAsArray ++ r.postorderTraversalAsArray) :+ contents 
    }
  }
  
  def breadthFirstTraversalAsArray(implicit m: ClassManifest[T]): Array[T] = {
    var returned = Array[T]()
    var visitQueue = new scala.collection.mutable.Queue[BinaryTree[T]]
    visitQueue.enqueue(this)
    while (!visitQueue.isEmpty) {
      returned :+ visitQueue.dequeue()
      if (l != null) visitQueue.enqueue(l)
      if (r != null) visitQueue.enqueue(r)
    }
    returned
  }
}

