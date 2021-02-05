package pfds

trait Heap[+T: Ordering, This[+T] <: Heap[T, ?]]:
  
  def isEmpty: Boolean

  def insert[S >: T: Ordering](x: S): This[S]

  def merge[S >: T: Ordering](that: This[S]): This[S]

  def min: T

  def deleteMin: This[T] 
  
