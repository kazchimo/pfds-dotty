given nothingOrdered: Ordering[Nothing] = new Ordering[Nothing]:
  override def compare(x: Nothing, y: Nothing): Int = 0
  
enum Tree[+T](implicit ord: Ordering[T]):
  case Leaf 
  case Node(left: Tree[T], elem: T,  right: Tree[T])(implicit ord: Ordering[T]) 
  
  def insert[S >: T](x: S)(implicit sord: Ordering[S]): Tree[S] = this match
    case Leaf => Node(Leaf, x, Leaf)
    case Node(l, y, r) => 
        if sord.gt(x, y) then 
            Node(l, y, r.insert(x))
        else if sord.lt(x, y) then
            Node(l.insert(x), y, r)
        else 
            this
end Tree
