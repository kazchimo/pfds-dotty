package set

import Color._

enum Color:
  case Red, Black

type BC = Black.type
type RC = Red.type

enum RedBlackSet[C <: Color, CL <: Color, +T, CR <: Color] extends Set[T]:
  case Empty extends RedBlackSet[BC, Nothing, Nothing, Nothing]
  case Node(color: C, left: RedBlackSet[CL, ?, T, ?], elem: T, right: RedBlackSet[CR, ?, T, ?])

  override type This[T] = RedBlackSet[C, CL, T, CR]

  override type Constraint[T] = Ordering[T]

  override def insert[S >: T: Ordering](x: S): RedBlackSet[C, CL, S, CR] = ???

  override def member[S >: T: Ordering](x: S): Boolean = this match
    case Empty => false
    case Node(_, l, e, r) =>
      if Ordering[S].lt(x, e) then l.member(x)
      else if Ordering[S].gt(x, e) then r.member(x)
      else true

end RedBlackSet

object RedBlackSet:
  def balance[T, C1 <: Color, C2 <: Color, C3 <: Color](
     color: C1, n1: RedBlackSet[C2, ?, T, ?], elem: T, n2: RedBlackSet[C3, ?, T, ?]
  ): RedBlackSet[?, ?, T, ?] = (color, n1, elem, n2) match
    case (Black, Node(Red, Node(Red, a, x, b), y, c), z, d) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, Node(Red, a, x, Node(Red, b, y, c)), z, d) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, a, x, Node(Red, Node(Red, b, y, c), z, d)) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (c, a, x, b) => Node(c, a, x, b)

  def just[C <: Color, T](c: C, x: T): RedBlackSet[C, BC, T, BC] = Node(c, Empty, x, Empty)

  def justBlack[T](x: T): RedBlackSet[BC, BC, T, BC] = just(Black, x)

  def justRed[T](x: T): RedBlackSet[RC, BC, T, BC] = just(Red, x)

  def black[T, C1 <: Color, C2 <: Color](
      left: RedBlackSet[C1, ?, T, ?], x: T, right: RedBlackSet[C2, ?, T, ?]
  ): RedBlackSet[BC, C1, T, C2] = Node(Black, left, x, right)

  def red[T, C1 <: Color, C2 <: Color](
      left: RedBlackSet[C1, ?, T, ?], x: T, right: RedBlackSet[C2, ?, T, ?]
  ): RedBlackSet[RC, C1, T, C2] = Node(Red, left, x, right)

  def withLeft[C <: Color, CL <: Color, T](
      c: C, left: RedBlackSet[CL, ?, T, ?], x: T): RedBlackSet[C, CL, T, ?] =
    Node(c, left, x, Empty)

  def withRight[C <: Color, T, CR <: Color](
      c: C, x: T, right: RedBlackSet[CR, ?, T, ?]): RedBlackSet[C, ?, T, CR] =
    Node(c, Empty, x, right)
end RedBlackSet
