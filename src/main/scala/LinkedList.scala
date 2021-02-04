enum LinkedList[+T]:
  case Nil extends LinkedList[Nothing]
  case Cons(x: T, s: LinkedList[T])

  def empty[T]: LinkedList[T] = Nil
