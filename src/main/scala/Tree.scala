package pfds

enum Tree[+T: Ordering]:
  case Leaf
  case Node(left: Tree[T], elem: T,  right: Tree[T])(implicit ord: Ordering[T])
  
  def insert[S >: T: Ordering](x: S): Tree[S] = this match
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
  given Ordering[Nothing] = new Ordering[Nothing]:
    override def compare(x: Nothing, y: Nothing): Int = 0

