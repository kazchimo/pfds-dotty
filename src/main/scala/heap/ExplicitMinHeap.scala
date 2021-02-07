package heap

enum ExplicitMinHeap[+T: Ordering, H[+T] <: Heap[T, H]] 
  extends Heap[T, [T] =>> ExplicitMinHeap[T, H]]:
  case Empty(heap: H[Nothing]) extends ExplicitMinHeap[Nothing, H]
  case NonEmpty(m: T, heap: H[T])(implicit ev: Ordering[T]) extends ExplicitMinHeap[T, H]

  type This[+T] = ExplicitMinHeap[T, H]

  override def min: T = this match
    case NonEmpty(min, _) => min
    case Empty => throw Exception("Empty Heap")

  override def isEmpty: Boolean = this match
    case Empty => true
    case NonEmpty => false

  /** Add a new element to this heap */
  override def insert[S >: T : Ordering](x: S): ExplicitMinHeap[S, H] = this match
    case Empty(h) => NonEmpty(x, h.insert(x))
    case NonEmpty(m, h) => 
      if Ordering[S].lteq(x, m) then NonEmpty(x, h.insert(x))
      else NonEmpty(m, h.insert(x))

  /** Merge two heaps */
  override def merge[S >: T : Ordering](that: ExplicitMinHeap[S, H]): ExplicitMinHeap[S, H] =
    (this, that) match
      case (Empty, _) => that
      case (_, Empty) => this
      case (NonEmpty(m1, h1), NonEmpty(m2, h2)) => 
        if Ordering[S].lteq(m1, m2) then NonEmpty(m1, h1.merge(h2))
        else NonEmpty(m2, h1.merge(h2))
      

  /** Delete the minimum value */
  override def deleteMin: ExplicitMinHeap[T, H] = this match
    case Empty => throw Exception("Empty Heap")
    case NonEmpty(m, h) =>
      val deleted = h.deleteMin
      if deleted.isEmpty then Empty(deleted.asInstanceOf[H[Nothing]])
      else NonEmpty(deleted.min, deleted)
      
