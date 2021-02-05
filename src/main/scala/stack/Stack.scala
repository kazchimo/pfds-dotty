package stack

trait Stack[+T, This[+T] <: Stack[T, ?]]:
  def isEmpty: Boolean

  def head: T

  def tail: This[T]
