package pfds

enum LinkedList[+T]:
  case Nil extends LinkedList[Nothing]
  case Cons(x: T, s: LinkedList[T])
  
object LinkedList:
  def empty[T]: LinkedList[T] = Nil
