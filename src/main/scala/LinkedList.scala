package pfds

enum LinkedList[+T]:
  case Nil extends LinkedList[Nothing]
  case Cons(x: T, s: LinkedList[T]) extends LinkedList[T]

  def isEmpty: Boolean = this match
    case Nil => true
    case _ => false

  def head: T = this match
    case Nil => throw Exception("Empty LinkedList")
    case Cons(x, _) => x

  def tail: LinkedList[T] = this match
    case Nil => throw Exception("Empty LinkedList")
    case Cons(_, s) => s

  def ++[S >: T](ys: LinkedList[S]): LinkedList[S] = this match
    case Nil => ys
    case Cons(x, s) => x :: (s ++ ys)
  
  def ::[S >: T](y: S): LinkedList[S] = Cons(y, this)

  def update[S >: T](i: Int, y: S): LinkedList[S] = this match
    case Nil => throw Exception("Empty LinkedList")
    case Cons(x, xs) if i == 0 => y :: xs
    case Cons(x, xs) if i > 0 => x :: xs.update(i - 1, y)
    case _ => throw Exception("Negative index update")


object LinkedList:
  def empty[T]: LinkedList[T] = Nil

  def cons[T](x: T, s: LinkedList[T]): LinkedList[T] = x :: s

  def apply[T](xs: T*): LinkedList[T] = xs match
    case Seq() => Nil
    case Seq(x, s: _*) => x :: apply(s: _*)
  
