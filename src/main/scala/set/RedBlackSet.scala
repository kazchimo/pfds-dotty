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
      
  def just[C <: Color, T](c: C, x: T): RedBlackSet[T, C] = Node(c, Empty, x, Empty)
  
  def justBlack[T](x: T): RedBlackSet[T, Black.type] = just(Black, x)
  
  def justRed[T](x: T): RedBlackSet[T, Red.type] = just(Red, x)
  
  def black[T](left: RedBlackSet[T, ?], x: T, right: RedBlackSet[T, ?]): RedBlackSet[T, Black.type] =
    Node(Black, left, x, right)
    
  def red[T](left: RedBlackSet[T, ?], x: T, right: RedBlackSet[T, ?]): RedBlackSet[T, Red.type] =
    Node(Red, left, x, right)
  
  def withLeft[C <: Color, T](c: C, left: RedBlackSet[T, ?], x: T): RedBlackSet[T, C] =
    Node(c, left, x, Empty)

  def withRight[C <: Color, T](c: C, x: T, right: RedBlackSet[T, ?]): RedBlackSet[T, C] =
    Node(c, Empty, x, right)
end RedBlackSet
