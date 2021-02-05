package set

trait Set[+T, This[+T] <: Set[T, ?]]:
  def insert[S >: T](x: S): This[S]

  def member[S >: T](x: S): Boolean
