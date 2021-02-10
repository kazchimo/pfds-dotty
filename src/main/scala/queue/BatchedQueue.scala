package queue

case class BatchedQueue[+T](private val front: List[T], private val rear: List[T]) 
  extends Queue[T, BatchedQueue]:
  override def isEmpty: Boolean = front.isEmpty 
  
  private def checkFront[S](front: List[S], rear: List[S]): BatchedQueue[S] = 
    if front.isEmpty then BatchedQueue(rear.reverse, Nil)
    else BatchedQueue(front, rear)

  /** Add a new element to the last position of the queue */
  override def :+[S >: T](a: S): BatchedQueue[S] = 
    checkFront(front, a :: rear)

  /** Take the first element of the queue */
  override def head: T = front match
    case Nil => throw new Exception("Empty Queue")
    case x :: xs => x

  /** The rest of the queue without its first element */
  override def tail: BatchedQueue[T] = front match
    case Nil => throw new Exception("Empty Queue")
    case x :: xs => checkFront(xs, rear)
  
