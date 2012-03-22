package trees

class BinaryTree[T: Comparable] {
  protected var contents: T = _
  protected var l: BinaryTree[T] = _
  protected var r: BinaryTree[T] = _
  
  def set(newval: T) = { contents = newval }
  def data = contents
  def leftChild = l
  def rightChild = r
  
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
}