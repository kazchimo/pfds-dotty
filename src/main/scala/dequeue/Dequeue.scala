package dequeue

trait Dequeue[+T, This[+T] <: Dequeue[T, ?]]:
  def isEmpty: Boolean
  
  def +:[S >: T](a: S): This[S]

  def head: T

  def tail: This[T]

  def :+[S >: T](a: S): This[S]

  def last: T

  def init: This[T]
