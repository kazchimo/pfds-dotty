package stack

trait Stack[+T]:
  def isEmpty: Boolean

  def head: T

  def tail: T
