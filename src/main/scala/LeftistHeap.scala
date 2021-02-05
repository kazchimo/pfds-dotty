/** Binary Tree which has keep-ordered and leftist property */
enum LeftistHeap[+T]:
  case Leaf
  case Node(rank: Int, elem: T, left: LeftistHeap[T], right: LeftistHeap[T])
