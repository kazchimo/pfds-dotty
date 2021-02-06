package set

trait Set[+T]:
  type This[+T] <: Set[T]
  type Constraint[_]
  
  def insert[S >: T: Constraint](x: S): This[S]

  def member[S >: T: Constraint](x: S): Boolean
