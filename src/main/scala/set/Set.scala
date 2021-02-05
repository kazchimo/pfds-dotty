package set

trait Set[+T, This[+T] <: Set[T, ?, ?], Constraint[_]]:
  def insert[S >: T: Constraint](x: S): This[S]

  def member[S >: T: Constraint](x: S): Boolean
