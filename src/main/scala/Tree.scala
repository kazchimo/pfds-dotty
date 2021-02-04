enum Tree[+T]:
  case Leaf extends Tree[Nothing]
  case Node(left: Tree[T], elem: T,  right: Tree[T]) extends Tree[T]

