package set

import Color._

enum Color:
  case Red, Black

enum RedBlackSet[+T, C <: Color] extends Set[T]:
  case Empty extends RedBlackSet[Nothing, Black.type]
  case Node(color: C, left: RedBlackSet[T, ?], elem: T, right: RedBlackSet[T, ?])

  override type This[T] = RedBlackSet[T, C]

  override type Constraint[T] = Ordering[T]

  override def insert[S >: T: Ordering](x: S): RedBlackSet[S, C] = ???

  override def member[S >: T: Ordering](x: S): Boolean = this match
    case Empty => false
    case Node(_, l, e, r) =>
      if Ordering[S].lt(x, e) then l.member(x)
      else if Ordering[S].gt(x, e) then r.member(x)
      else true

end RedBlackSet

object RedBlackSet:
  def balance[T, C1 <: Color, C2 <: Color, C3 <: Color](
     color: C1, n1: RedBlackSet[T, C2], elem: T, n2: RedBlackSet[T, C3]
  ): RedBlackSet[T, ?] = (color, n1, elem, n2) match
    case (Black, Node(Red, Node(Red, a, x, b), y, c), z, d) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, Node(Red, a, x, Node(Red, b, y, c)), z, d) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, a, x, Node(Red, Node(Red, b, y, c), z, d)) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (c, a, x, b) => Node(c, a, x, b)
end RedBlackSet
