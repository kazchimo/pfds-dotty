package heap

import heap.Heap
import heap.LeftistHeap._
import LeftistHeap._

import scala.annotation.tailrec

/**
 * Binary Tree which is heap-ordered and have leftist property.
 *
 * heap-ordered:
 *   Any parent nodes cannot be greater than child nodes.
 *   Thus smallest value is on the root of heap.
 * leftist property:
 *   Any right child node's `rank` is smaller than or equal to any left child node's one.
 *   Which means right spine is shortest way to empty node.
 * */
enum LeftistHeap[+T: Ordering] extends Heap[T, LeftistHeap]:
  case Leaf // Empty node
  case Node(r: Int, elem: T, left: LeftistHeap[T], right: LeftistHeap[T])(implicit ord: Ordering[T])

  /**
   * Rank of LeftistHeap which represents a length of right spine.
   * Right spine is the rightest path to empty Node.
   * So should return (number of values on right spine) + 1
   *
   * Figure:
   *       .
   *      / \    .rank
   *     .   .  ------->  2
   *    /
   *   .
   * */
  def rank: Int = this match
    case Leaf => 0
    case Node(r, _, _, _) => r

  /** Merge two LeftistHeap in O(log n) order */
  def merge[S >: T](that: LeftistHeap[S])(using sord: Ordering[S]): LeftistHeap[S] =
    (this, that) match
      case (Leaf, l) => l
      case (l, Leaf) => l
      case (Node(_, x, a1, b1), Node(_, y, a2, b2)) =>
        if sord.lteq(x, y) then
          Node(x, a1, b1.merge(that))
        else Node(y, a2, b2.merge(this))

  /** Insert `x` into LeftistHeap in O(log n) order */
  def insert[S >: T: Ordering](x: S): LeftistHeap[S] =
    just(x).merge(this)

  /** Return a minimum value in this LeftistHeap in O(1) order */
  def min: T = this match
    case Leaf => throw Exception("Empty node")
    case Node(_, x, _, _) => x

  /** Delete a minimum value in this LeftistHeap in O(log n) order */
  def deleteMin: LeftistHeap[T] = this match
    case Leaf => throw Exception("Empty node")
    case Node(_, _, a, b) => a.merge(b)

  def isEmpty: Boolean = this match
    case Leaf => true
    case _ => false
end LeftistHeap

object LeftistHeap:
  def empty[T]: LeftistHeap[T] = Leaf

  /**
   * Create a LeftistHeap containing just one value
   * Figure:
   *   .
   * */
  def just[T: Ordering](x: T): LeftistHeap[T] = Node(1, x, Leaf, Leaf)

  /**
   * Create a LeftistHeap containing three values as triangle ordering values
   * Figure:
   *     .
   *    / \
   *   .   .
   * */
  def triangle[T: Ordering](a: T, b: T, c: T): LeftistHeap[T] = {
    val List(min, a1, a2) = List(a, b, c).sorted
    Node(min, just(a1), just(a2))
  }

  /** Create LeftistHeap from a List in O(n) order */
  def fromList[T: Ordering](ls: List[T]): LeftistHeap[T] = {
    @tailrec def mergeList(merged: List[LeftistHeap[T]], rest: List[LeftistHeap[T]]): LeftistHeap[T] =
      (merged, rest) match
        case (h :: Nil, Nil) => h // all heaps are merged and remains no rest values
        case (hs, Nil) => mergeList(Nil, hs) // all heaps are merged in this cycle but heaps not converged to one
        case (hs, x::Nil) => mergeList(Nil, x::hs) // remains one heap to be merged and pass it to next cycle
        case (hs, x1::x2::xs) => mergeList(x1.merge(x2)::hs ,xs) // merge two values

    mergeList(Nil, ls.map(just))
  }

  object Node:
    /** Create a Node calculating the rank and sorting child Nodes */
    def apply[T: Ordering](x: T, a: LeftistHeap[T], b: LeftistHeap[T]): Node[T] =
      if a.rank >= b.rank then Node(b.rank + 1, x, a, b) else Node(a.rank + 1, x, b, a)
end LeftistHeap

