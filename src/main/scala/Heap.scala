package pfds

/** A data structure which provide an efficient access to a minimum value */
trait Heap[+T: Ordering, This[+T] <: Heap[T, ?]]:
  
  def isEmpty: Boolean

  def insert[S >: T: Ordering](x: S): This[S]

  def merge[S >: T: Ordering](that: This[S]): This[S]

  def min: T

  def deleteMin: This[T] 
  
