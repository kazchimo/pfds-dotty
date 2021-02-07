package heap

private[heap] object TreeListOps:
  extension [T: Ordering](l: List[BinomialTree[T]])
    def  toHeap = BinomialHeap(l)

private[heap] case class BinomialTree[+T: Ordering](rank: Int, elem: T, childs: List[BinomialTree[T]]):
  /** Create a new (`rank` + 1) BinomialTree making `this` as a child of `that` */
  def asChildOf[S >: T: Ordering](that: BinomialTree[S]): BinomialTree[S] =
    BinomialTree(rank + 1, that.elem, this :: that.childs)

  /** Create a new (`rank` + 1) BinomialTree linking `this` with `that` */
  def link[S >: T](that: BinomialTree[S])(using sord: Ordering[S]): BinomialTree[S] =
    if sord.lteq(elem, that.elem) then that.asChildOf(this)
    else this.asChildOf(that)
end BinomialTree

object BinomialTree:
  /** Create a rank 0 BinomialTree */
  def just[T: Ordering](x: T): BinomialTree[T] = BinomialTree(0, x, Nil)

  /**
   * Figure:
   *   *
   *
   * */
  def rank0[T: Ordering](a: T): BinomialTree[T] = just(a)

  /**
   * Figure:
   *   *
   *   |
   *   *
   *
   * */
  def rank1[T: Ordering](a: T, b: T): BinomialTree[T] =
    if summon[Ordering[T]].lteq(a, b) then BinomialTree(1, a, List(just(b)))
    else BinomialTree(1, b, List(just(a)))

  /**
   * Figure:
   *  _rank1    __rank1           *
   *     *         *             /|
   *     |    +    |     ==>    * *
   *     *         *            |
   *                            *
   *
   * */
  def rank2[T: Ordering](_rank1: (T, T), __rank1: (T, T)): BinomialTree[T] = {
    val t1 = rank1(_rank1._1, _rank1._2)
    val t2 = rank1(__rank1._1, __rank1._2)
    t1.link(t2)
  }
end BinomialTree


case class BinomialHeap[+T: Ordering] private[heap] (trees: List[BinomialTree[T]]) extends Heap[T, BinomialHeap]:
  import TreeListOps._

  def ::[S >: T: Ordering](tree: BinomialTree[S]): BinomialHeap[S] = (tree :: trees).toHeap

  override def isEmpty: Boolean = trees.isEmpty

  /**
   * Insert a BinomialTree into this heap.
   * This operation corresponds to the addition in binary digit.
   * Takes O(log n) order time in the worst case when size of heap is 2^k - 1 to link trees.
   * */
  def insTree[S >: T: Ordering](tree: BinomialTree[S]): BinomialHeap[S] = this.trees match
    case Nil => BinomialHeap(tree)
    case h :: tail =>
      if tree.rank < h.rank then tree :: this
      else tail.toHeap.insTree(tree.link(h))

  /** Inset a value into this heap */
  override def insert[S >: T : Ordering](x: S): BinomialHeap[S] =
    insTree(BinomialTree.rank0(x))

  /** Merge two heaps */
  override def merge[S >: T : Ordering](that: BinomialHeap[S]): BinomialHeap[S] =
    (this.trees, that.trees) match
      case (Nil, _)  => that
      case (_, Nil) => this
      case (h1 :: t1, h2 :: t2) =>
        if h1.rank < h2.rank then h1 :: t1.toHeap.merge(that)
        else if h2.rank < h1.rank then h2 :: this.merge(t2.toHeap)
        else t1.toHeap.merge(t2.toHeap).insTree(h1.link(h2))
        
  /** Returns a pair of the tree which has minimum value in heap and rest heap */
  def removeMinTree: (BinomialTree[T], BinomialHeap[T]) = trees match
    case Nil => throw Exception("Empty Heap")
    case t :: Nil => (t, Nil.toHeap)
    case t :: ts =>
      val (t2, ts2) = ts.toHeap.removeMinTree
      if Ordering[T].lteq(t.elem, t2.elem) then (t, ts.toHeap)
      else (t2, t :: ts2)

  override def min: T = removeMinTree._1.elem

  /** Create a new heap deleteing the minimum value */
  override def deleteMin: BinomialHeap[T] = {
    val (BinomialTree(_, _, ts1), ts2) = removeMinTree
    // child trees of a tree should be reversed because they are sorted by rank in desc order
    ts1.reverse.toHeap.merge(ts2) 
  }
end BinomialHeap

object BinomialHeap:
  def empty[T: Ordering]: BinomialHeap[T] = BinomialHeap(List())

  /** Create a BinomialHeap with BinomialTrees sorted by rank in ascending order */
  def apply[T: Ordering](trees: BinomialTree[T]*): BinomialHeap[T] =
    BinomialHeap(trees.sortBy(_.rank).toList)
end BinomialHeap
