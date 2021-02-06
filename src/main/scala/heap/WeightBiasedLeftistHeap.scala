package heap

import heap.Heap
import heap.WeightBiasedLeftistHeap._
import WeightBiasedLeftistHeap._

/** Binary Tree which has weight-biased leftist property */
enum WeightBiasedLeftistHeap[+T: Ordering] extends Heap[T]:
  case Leaf // Empty Node
  case Node(
         r: Int,
         elem: T,
         left: WeightBiasedLeftistHeap[T],
         right: WeightBiasedLeftistHeap[T])(implicit ord: Ordering[T])
  
  type This[T] = WeightBiasedLeftistHeap[T]

  def weight: Int = this match
    case Leaf => 0
    case Node(r, _, _, _) => r
  
  def weightSum[S >: T](that: WeightBiasedLeftistHeap[S]): Int =
    this.weight + that.weight

  override def isEmpty: Boolean = this match
    case Leaf => true
    case _ => false

  override def insert[S >: T : Ordering](x: S): WeightBiasedLeftistHeap[S] = just(x).merge(this)

  override def merge[S >: T](
    that: WeightBiasedLeftistHeap[S])(using sord: Ordering[S]): WeightBiasedLeftistHeap[S] = (this, that) match
    case (Leaf, h) => h
    case (h, Leaf) => h
    case (Node(_, x, a1, b1), Node(_, y, a2, b2)) =>
      val weightSum = this.weightSum(that)
      
      if sord.lteq(x, y) then { // `that` should be under `this`
        if b1.weightSum(that) >= a1.weight then
          Node(weightSum, x, b1.merge(that), a1)
        else Node(weightSum, x, a1, b1.merge(that))
      } else { // `this` should be under `that`
        if b2.weightSum(that) >= a2.weight then
          Node(weightSum, y, b2.merge(this), a2)
        else Node(weightSum, y, a2, b2.merge(this))
      }

  override def min: T = this match
    case Leaf => throw Exception("Empty node")
    case Node(_, x, _, _) => x

  override def deleteMin: WeightBiasedLeftistHeap[T] = this match
    case Leaf => throw Exception("Empty node")
    case Node(_, _, a, b) => a.merge(b)

end WeightBiasedLeftistHeap

object WeightBiasedLeftistHeap:
  def empty[T: Ordering]: WeightBiasedLeftistHeap[T] = Leaf

  def just[T: Ordering](x: T): WeightBiasedLeftistHeap[T] = Node(1, x, Leaf, Leaf)

  def triangle[T: Ordering](a: T, b: T, c: T): WeightBiasedLeftistHeap[T] = {
    val List(min, a1, a2) = List(a, b, c).sorted
    Node(min, just(a1), just(a2))
  }

  object Node:
    def apply[T: Ordering](
      x: T, a: WeightBiasedLeftistHeap[T], b: WeightBiasedLeftistHeap[T]
    ): WeightBiasedLeftistHeap[T] =
      if a.weight >= b.weight then
        Node(b.weight + a.weight + 1, x, a, b)
      else Node(a.weight + b.weight + 1, x, b, a)
