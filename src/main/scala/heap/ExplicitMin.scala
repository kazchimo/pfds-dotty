package heap

enum ExplicitMin[H[+T] <: Heap[T]]:
  case Empty extends ExplicitMin[Nothing]
  case NonEmpty[T: Ordering, H[+T] <: Heap[T]](heap: H[T])  extends ExplicitMin[H]
