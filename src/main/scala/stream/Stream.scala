package stream

trait Stream[+T]:
  def ++[S >: T](that: Stream[S]): Stream[S]

  def take(n: Int): Stream[T]

  def drop(n: Int): Stream[T]

  def reverse: Stream[T]

