package trees

class BinarySearchTree[T: Ordered] extends BinaryTree[T] {
  // FIXME is there a more elegant way to do this?
  var l: BinarySearchTree[T] = _
  var r: BinarySearchTree[T] = _
  
  def insert(root: BinarySearchTree[T], newval: T) {
    if (root == null) {
      var newnode = new BinarySearchTree[T]
      newnode.set(newval)
      newnode
    } else {
      if (newval <= root.data) {
        l = insert(root.l, newval)
      } else {
        r = insert(root.r, newval)
      }
    }
  }
} 