package set

import Color._

enum Color:
  case Red, Black

type ChildColor[C <: Color] <: Color = C match
  case Red.type => Black.type
  case Black.type => Red.type | Black.type

enum RedBlackSet[+T, C <: Color] extends Set[T]:
  case Empty extends RedBlackSet[Nothing, Black.type]
  case Node(color: C, left: RedBlackSet[T, ChildColor[C]], elem: T, right: RedBlackSet[T, ChildColor[C]])
  
  override type This[T] = RedBlackSet[T, C]
  
  override type Constraint[T] = Ordering[T]

  override def insert[S >: T: Ordering](x: S): RedBlackSet[S, C] = ???

  override def member[S >: T: Ordering](x: S): Boolean = ???
end RedBlackSet
