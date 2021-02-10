package dequeue

case class TwoListDequeue[+T](front: List[T], rear: List[T]) extends Dequeue[T, TwoListDequeue]:
  override def isEmpty: Boolean = (front, rear) match
    case (Nil, Nil) => true
    case _ => false
  
  override def +:[S >: T](a: S): TwoListDequeue[S] = checkRear(a :: front, rear)

  override def head: T = (front, rear) match
    case (Nil, Nil) => throw new Exception("Empty Dequeue")
    // only `x :: Nil` will be matched but wirtten as this for match exhaustivity
    case (Nil, x :: xs) => x 
    case (x :: xs, _) => x

  override def tail: TwoListDequeue[T] = (front, rear) match
    case (Nil, Nil) => throw Exception("Empty Dequeue")
    // only `x :: Nil` will be matched but wirtten as this for match exhaustivity
    case (Nil, x :: xs) => TwoListDequeue.empty
    case (x :: xs, r) => checkFront(xs, r)

  override def :+[S >: T](a: S): TwoListDequeue[S] = checkFront(front, a :: rear)

  override def last: T = (front, rear) match
    case (Nil, Nil) => throw Exception("Empty Dequeue")
    // only `x :: Nil` will be matched but wirtten as this for match exhaustivity
    case (x :: xs, Nil) => x
    case (f, x :: xs) => x

  override def init: TwoListDequeue[T] = (front, rear) match
    case (Nil, Nil) => throw new Exception("Empty Dequeue")
    // only `x :: Nil` will be matched but wirtten as this for match exhaustivity
    case (x :: xs, Nil) => TwoListDequeue.empty
    case (f, x :: xs) => checkRear(f, xs)
  
  def reverse: TwoListDequeue[T] = TwoListDequeue(rear, front)
  
  private def checkFront[S](front: List[S], rear: List[S]): TwoListDequeue[S] =
    (front, rear) match
      case (Nil, xs) => TwoListDequeue(xs.take(xs.length / 2).reverse, xs.drop(xs.length / 2))
      case _ => TwoListDequeue(front, rear)

  private def checkRear[S](front: List[S], rear: List[S]): TwoListDequeue[S] =
    checkFront(rear, front).reverse
end TwoListDequeue

object TwoListDequeue:
  def empty[T]: TwoListDequeue[T] = TwoListDequeue(Nil, Nil)