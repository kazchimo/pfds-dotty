package pfds

import WeightBiasedLeftistHeap._

enum WeightBiasedLeftistHeap[+T: Ordering] extends Heap[T, WeightBiasedLeftistHeap]:
  case Leaf // Empty Node
  case Node(
         r: Int,
         elem: T,
         left: WeightBiasedLeftistHeap[T],
         right: WeightBiasedLeftistHeap[T])(implicit ord: Ordering[T])
  
  def weight: Int = this match
    case Leaf => 0
    case Node(r, _, _, _) => r
  
  override def isEmpty: Boolean = this match
    case Leaf => true
    case _ => false

  override def insert[S >: T : Ordering](x: S): WeightBiasedLeftistHeap[S] = just(x).merge(this)

  override def merge[S >: T](
    that: WeightBiasedLeftistHeap[S])(using sord: Ordering[S]): WeightBiasedLeftistHeap[S] = (this, that) match
    case (Leaf, h) => h
    case (h, Leaf) => h
    case (Node(_, x, a1, b1), Node(_, y, a2, b2)) =>
      if sord.lteq(x, y) then
        Node(x, a1, b1.merge(that))
      else Node(y, a2, b2.merge(this))
  
  override def min: T = this match
    case Leaf => throw Exception("Empty node")
    case Node(_, x, _, _) => x

  override def deleteMin: WeightBiasedLeftistHeap[T] = this match
    case Leaf => throw Exception("Empty node")
    case Node(_, _, a, b) => a.merge(b)
      
end WeightBiasedLeftistHeap

object WeightBiasedLeftistHeap:
  def just[T: Ordering](x: T): WeightBiasedLeftistHeap[T] = Node(3, x, Leaf, Leaf)
  
  object Node:
    def apply[T: Ordering](
      x: T, a: WeightBiasedLeftistHeap[T], b: WeightBiasedLeftistHeap[T]
    ): WeightBiasedLeftistHeap[T] =
      if a.weight >= b.weight then 
        Node(b.weight + a.weight + 1, x, a, b) 
      else Node(a.weight + b.weight + 1, x, b, a)
