package stream

import SimpleStream._
import scala.annotation.targetName

enum SimpleStream[+T] extends Stream[T, SimpleStream] {
  case SNil
  case SCons(h: T, tl: () => SimpleStream[T])
  
  def head: T = this match
    case SNil => throw new Exception("Empty Stream")
    case SCons(head, tail) => head
  
  def tail: SimpleStream[T] = this match
    case SNil => throw Exception("Empty Stream")
    case SCons(head, tail) => tail()

  override def ++[S >: T](that: SimpleStream[S]): SimpleStream[S] = this match
    case SNil => that
    case SCons(head, tail) => SimpleStream(head, tail() ++ that)

  override def take(n: Int): SimpleStream[T] = this match
    case SNil => SNil
    case _ if n <= 0 => SNil
    case SCons(head, tail) => SimpleStream(head, tail().take(n - 1))

  override def drop(n: Int): SimpleStream[T] = this match
    case SNil => SNil
    case _ if n <= 0 => this
    case SCons(head, tail)=> tail().drop(n - 1)

  override def reverse: SimpleStream[T] = {
    def rev(s1: => SimpleStream[T], s2: => SimpleStream[T]): SimpleStream[T] = s1 match
      case SNil => s2
      case SCons(head, tail) => rev(tail(), SimpleStream(head, s2))

    rev(this, SNil)
  }
}

object SimpleStream:
  object SCons:
    def apply[T](head: T, tail: => SimpleStream[T]): SimpleStream[T] = SCons(head, () => tail)
  
  def apply[T](h: T, t: => SimpleStream[T]): SimpleStream[T] = SCons(h, t)
      
  def repeat[T](a: T): SimpleStream[T] = SCons(a, repeat(a))
end SimpleStream
