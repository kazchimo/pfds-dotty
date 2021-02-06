package stack

/** Stack using builtin List */
case class StdList[+T](private val l: List[T]) extends Stack[T]:
  type This[T] = StdList[T]
  
  override def isEmpty: Boolean = l.isEmpty

  override def head: T = l.head

  override def tail: StdList[T] = StdList(l.tail)

