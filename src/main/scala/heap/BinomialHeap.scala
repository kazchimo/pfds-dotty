package heap

private[heap] case class BinomialTree[+T: Ordering](rank: Int, elem: T, childs: List[BinomialTree[T]]):
  /** Create a new (`rank` + 1) BinomialTree making `this` as a child of `that` */
  def asChildOf[S >: T: Ordering](that: BinomialTree[S]): BinomialTree[S] =
    BinomialTree(rank + 1, that.elem, this :: that.childs)
  
  /** Create a new (`rank` + 1) BinomialTree linking `this` with `that` */
  def link[S >: T](that: BinomialTree[S])(using sord: Ordering[S]): BinomialTree[S] =
    if sord.lteq(elem, that.elem) then that.asChildOf(this)
    else this.asChildOf(that)


object BinomialTree:
  /** Create a rank 0 BinomialTree */
  def just[T: Ordering](x: T): BinomialTree[T] = BinomialTree(0, x, Nil)

  def rank0[T: Ordering](a: T): BinomialTree[T] = just(a)

  def rank1[T: Ordering](a: T, b: T): BinomialTree[T] =
    if summon[Ordering[T]].lteq(a, b) then BinomialTree(1, a, List(just(b)))
    else BinomialTree(1, b, List(just(a)))

  def rank2[T: Ordering](a: T, b: T, c: T, d: T): BinomialTree[T] = {
    val List(a1, a2, a3, a4) = List(a, b, c, d).sorted
    BinomialTree(2, a1, List(BinomialTree(1, a3, List(just(a4))), just(a2)))
  }


case class BinomialHeap[+T: Ordering](private val trees: List[BinomialTree[T]]) extends Heap[T]:
  override type This[T] = BinomialHeap[T]

  override def isEmpty: Boolean = trees.isEmpty

  override def insert[S >: T : Ordering](x: S): BinomialHeap[S] = ???

  override def merge[S >: T : Ordering](that: BinomialHeap[S]): BinomialHeap[S] = ???

  override def min: T = ???

  override def deleteMin: BinomialHeap[T]  = ???

object BinomialHeap:
  def empty[T: Ordering] = BinomialHeap(List())
end BinomialHeap