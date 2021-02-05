enum LeftistHeap[+T]:
  case Leaf
  case Node(rank: Int, elem: T, left: LeftistHeap[T], right: LeftistHeap[T])
  


