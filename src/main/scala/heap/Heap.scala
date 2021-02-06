package heap

/** A data structure which provide an efficient access to a minimum value */
trait Heap[+T: Ordering]:
  type This[+T] <: Heap[T]

  def isEmpty: Boolean

  /** Add a new element to this heap */
  def insert[S >: T: Ordering](x: S): This[S]

  /** Merge two heaps */
  def merge[S >: T: Ordering](that: This[S]): This[S]

  /** The minimum value */
  def min: T

  /** Delete the minimum value */
  def deleteMin: This[T] 
  
