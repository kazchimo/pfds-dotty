package stream

trait Stream[+T, This[+T] <: Stream[T, ?]]:
  def ++[S >: T](that: This[S]): This[S]

  def take(n: Int): This[T]

  def drop(n: Int): This[T]

  def reverse: This[T]

