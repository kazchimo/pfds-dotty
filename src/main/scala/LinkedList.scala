package pfds

enum LinkedList[+T]:
  case Nil extends LinkedList[Nothing]
  case Cons(x: T, s: LinkedList[T])

  def isEmpty = this match {
    case Nil => true
    case _ => false
  }
  
object LinkedList:
  def empty[T]: LinkedList[T] = Nil
