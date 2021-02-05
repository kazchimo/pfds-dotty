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
  case Leaf
  case Node(r: Int, elem: T, left: LeftistHeap[T], right: LeftistHeap[T])(implicit ord: Ordering[T])

  /**
   * Rank of LeftistHeap which represents a length of right spine
   * Figure:
   *       .
   *      / \    .rank
   *     .   .  ------->  1
   *    /
   *   .
   * */
  def rank = this match
    case Leaf => 0
    case Node(r, _, _, _) => r
end LeftistHeap

object LeftistHeap:
  /** Create a LeftistHeap containing just one value */
  def just[T: Ordering](x: T): LeftistHeap[T] = Node(1, x, Leaf, Leaf)

  object Node:
    /** Create a Node calculating the rank and sorting child Nodes */
    def apply[T: Ordering](x: T, a: LeftistHeap[T], b: LeftistHeap[T]): Node[T] =
      if a.rank >= b.rank then Node(b.rank + 1, x, a, b) else Node(a.rank + 1, x, b, a)
end LeftistHeap

