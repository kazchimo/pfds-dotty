package pfds

import scala.annotation.tailrec
import Tree._

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
      case Node(_, y, _) => go(y, this)
  }

  def insert[S >: T](x: S)(using sord: Ordering[S]): Tree[S] = {
    def go(cand: S, t: Tree[S]): Tree[S] = t match
      case Leaf => if cand == x then throw SameValue else Node(Leaf, x, Leaf)
      case Node(l, y, r) =>
        if sord.gt(x, y) then
          Node(l, y, go(cand, r))
        else
          Node(go(y, l), y, r)

    try {
      this match
        case Leaf => Tree.just(x)
        case Node(_, y, _) => go(y, this)
    } catch {
      case SameValue => this
    }
  }
end Tree


object Tree:
  given Ordering[Nothing] = new Ordering[Nothing] :
    override def compare(x: Nothing, y: Nothing): Int = 0

  /* Create a Tree containing just one value 
   * Figure:
   *  x
   **/
  def just[T: Ordering](x: T): Tree[T] = Node(Leaf, x, Leaf)

  def same[T: Ordering](x: T): Tree[T] = Node(just(x), x, just(x))

  def withLeft[T: Ordering](l: Tree[T], e: T): Tree[T] = Node(l, e, Leaf)

  def withRight[T: Ordering](e: T, r: Tree[T]): Tree[T] = Node(Leaf, e, r)

  def complete[T: Ordering](x: T, d: Int): Tree[T] = d match
    case 0 => Leaf
    case _ if d > 0 =>
      val cmp = complete(x, d - 1)
      Node(cmp, x, cmp)
    case _ => throw Exception("Negative depth of Tree")

  /** Create balanced Tree with O(log n) order */
  def balanced[T: Ordering](x: T, n: Int): Tree[T] = {
    def balanced2(m: Int) = (balanced(x, m), balanced(x, m + 1))

    n match
      case 0 => Leaf
      case _ if n > 0 & n % 2 == 1 =>
        val t = balanced(x, (n - 1) / 2)
        Node(t, x, t)
      case _ if n > 0 & n % 2 == 0 =>
        val (l, r) = balanced2((n - 1) / 2)
        Node(l, x, r)
  }


  object SameValue extends Exception
end Tree

