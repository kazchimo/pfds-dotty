package set

import Color._
import RedBlackSet._

enum Color:
  case Red, Black


enum RedBlackSet[+T] extends Set[T]:
  case Empty extends RedBlackSet[Nothing]
  case Node(color: Color, left: RedBlackSet[T], elem: T, right: RedBlackSet[T])

  override type This[T] = RedBlackSet[T]

  override type Constraint[T] = Ordering[T]

  override def insert[S >: T: Ordering](x: S): RedBlackSet[S] = {
    def ins(s: RedBlackSet[S]): RedBlackSet[S] = s match
      case Empty => justRed(x)
      case Node(color, a, y, b) => 
        if Ordering[S].lt(x, y) then balance(color, ins(a), y, b)
        else if Ordering[S].gt(x, y) then balance(color, a, y, ins(b))
        else s
    
    val Node(_, a, y, b) = ins(this)
    Node(Black, a, y, b)
  }

  override def member[S >: T: Ordering](x: S): Boolean = this match
    case Empty => false
    case Node(_, l, e, r) =>
      if Ordering[S].lt(x, e) then l.member(x)
      else if Ordering[S].gt(x, e) then r.member(x)
      else true

end RedBlackSet

object RedBlackSet:
  def balance[T](
     color: Color, n1: RedBlackSet[T], elem: T, n2: RedBlackSet[T]
  ): RedBlackSet[T] = (color, n1, elem, n2) match
    case (Black, Node(Red, Node(Red, a, x, b), y, c), z, d) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, Node(Red, a, x, Node(Red, b, y, c)), z, d) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, a, x, Node(Red, Node(Red, b, y, c), z, d)) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (c, a, x, b) => Node(c, a, x, b)

  def just[T](c: Color, x: T): RedBlackSet[T] = Node(c, Empty, x, Empty)

  def justBlack[T](x: T): RedBlackSet[T] = just(Black, x)

  def justRed[T](x: T): RedBlackSet[T] = just(Red, x)

  def black[T](left: RedBlackSet[T], x: T, right: RedBlackSet[T]): RedBlackSet[T] =
    Node(Black, left, x, right)

  def red[T](left: RedBlackSet[T], x: T, right: RedBlackSet[T]): RedBlackSet[T] =
    Node(Red, left, x, right)

  def withLeft[T](c: Color, left: RedBlackSet[T], x: T): RedBlackSet[T] =
    Node(c, left, x, Empty)

  def withRight[T](c: Color, x: T, right: RedBlackSet[T]): RedBlackSet[T] =
    Node(c, Empty, x, right)
end RedBlackSet
