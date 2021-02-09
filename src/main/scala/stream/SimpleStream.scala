package stream

import SimpleStream._
import scala.annotation.targetName

enum SimpleStream[+T] extends Stream[T] {
  case SNil
  case SCons(head: T, tail: () => Stream[T])

  override def ++[S >: T](that: Stream[S]): Stream[S] = this match
    case SNil => that
    case SCons(head, tail) => SimpleStream(head, tail() ++ that)

  override def take(n: Int): Stream[T] = this match
    case SNil => SNil
    case _ if n <= 0 => SNil
    case SCons(head, tail) => SimpleStream(head, tail().take(n - 1))

  override def drop(n: Int): Stream[T] = this match
    case SNil => SNil
    case _ if n <= 0 => this
    case SCons(head, tail)=> SimpleStream(head, tail().drop(n - 1))

  override def reverse: Stream[T] = {
    def rev(s1: Stream[T], s2: Stream[T]): Stream[T] = s1 match
      case SNil => s2
      case SCons(head, tail) => rev(tail(), SimpleStream(head, s2))

    rev(this, SNil)
  }
}

object SimpleStream:
  object SCons:
    def apply[T](head: T, tail: => Stream[T]): SimpleStream[T] = SCons(head, () => tail)
  
  def apply[T](h: T, t: => Stream[T]): SimpleStream[T] = SCons(h, t)
  
  def apply[T](as: T*): SimpleStream[T] = as match
    case Seq() => SNil
    case Seq(h, as: _*) => SCons(h, () => apply(as: _*))
      
  def repeat[T](a: T): SimpleStream[T] = SCons(a, repeat(a))
end SimpleStream
