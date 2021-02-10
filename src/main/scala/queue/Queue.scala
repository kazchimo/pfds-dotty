package queue

trait Queue[+T, This[+T] <: Queue[T, ?]]:
  def isEmpty: Boolean

  /** Add a new element to the last position of the queue */
  def :+[S >: T](a: S): This[S]

  /** Take the first element of the queue */
  def head: T

  /** The rest of the queue without its first element */
  def tail: This[T]

