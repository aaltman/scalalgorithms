package trees
import scala.NotDefinedError
import scala.reflect._

class BinarySearchTree[T <: Ordered[T]](stored_value: T, val TFac: () => T) extends BinaryTree(stored_value) {  
  def insert(newval: T) {
	if (newval <= contents) {
	  if (l == null) {
	  	throw new NotDefinedError("Still trying to work around type erasure.")
	  } else {
	  	l.insert(newval)
	  }
	} else {
	  if (r == null) {
	    throw new NotDefinedError("Still trying to work around type erasure.")
	  }
	  r.insert(newval)
	}
  }
  
  def delete(newval: T) {
    throw new NotDefinedError("Haven't yet defined binary search tree delete method.")
  }
} 