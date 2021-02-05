package pfds
import LeftistHeap._

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
enum LeftistHeap[+T: Ordering]:
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
  def rank = this match
    case Leaf => 0
    case Node(r, _, _, _) => r
      
  /** Merge two LeftistHeap with O(log n) order */
  def merge[S >: T](that: LeftistHeap[S])(using sord: Ordering[S]): LeftistHeap[S] =
    (this, that) match
      case (Leaf, l) => l
      case (l, Leaf) => l
      case (Node(_, x, a1, b1), Node(_, y, a2, b2)) => 
        if sord.lteq(x, y) then 
          Node(x, a1, b1.merge(that)) 
        else Node(y, a2, b2.merge(this))
end LeftistHeap

object LeftistHeap:
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

  object Node:
    /** Create a Node calculating the rank and sorting child Nodes */
    def apply[T: Ordering](x: T, a: LeftistHeap[T], b: LeftistHeap[T]): Node[T] =
      if a.rank >= b.rank then Node(b.rank + 1, x, a, b) else Node(a.rank + 1, x, b, a)
end LeftistHeap

