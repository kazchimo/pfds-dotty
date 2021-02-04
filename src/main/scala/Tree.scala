package pfds

import scala.annotation.tailrec

enum Tree[+T: Ordering]:
  case Leaf
  case Node(left: Tree[T], elem: T, right: Tree[T])(implicit ord: Ordering[T])
  
  def member[S >: T](x: S)(using sord: Ordering[S]): Boolean = {
    @tailrec def go(cand: S, t: Tree[S]): Boolean = t match
      case Leaf => cand == x
      case Node(l, y, r) =>
        if sord.lt(x, y) then
          go(cand, l)
        else
          go(y, r)
          
    this match
      case Leaf => false
      case Node(l, y, r) => if sord.gt(x, y) then l.member(x) else go(y, r)
  }

  def insert[S >: T : Ordering](x: S): Tree[S] = this match
    case Leaf => Node(Leaf, x, Leaf)
    case Node(l, y, r) =>
      if summon[Ordering[S]].gt(x, y) then
        Node(l, y, r.insert(x))
      else if summon[Ordering[S]].lt(x, y) then
        Node(l.insert(x), y, r)
      else
        this
end Tree

object Tree:
  given Ordering[Nothing] = new Ordering[Nothing] :
    override def compare(x: Nothing, y: Nothing): Int = 0

  def just[T: Ordering](x: T): Tree[T] = Node(Leaf, x, Leaf)

