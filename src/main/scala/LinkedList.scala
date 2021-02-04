package pfds

enum LinkedList[+T]:
  case Nil extends LinkedList[Nothing]
  case Cons(x: T, s: LinkedList[T])

  def isEmpty: Boolean = this match {
    case Nil => true
    case _ => false
  }

  def head: T = this match {
    case Nil => throw Exception("Empty LinkedList")
    case Cons(x, _) => x
  }

  def tail: LinkedList[T] = this match {
    case Nil => throw Exception("Empty LinkedList")
    case Cons(_, s) => s
  }

object LinkedList:
  def empty[T]: LinkedList[T] = Nil

  def cons[T](x: T, s: LinkedList[T]): LinkedList[T] = LinkedList.Cons(x, s)
