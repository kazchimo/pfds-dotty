package pfds

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import LeftistHeap._

class LeftistHeapSpec extends AnyFunSuite with Matchers:
  test("#merge") {
    LeftistHeap.just(1).merge(LeftistHeap.triangle(2, 3, 4)) shouldBe 
      Node(1, 1, LeftistHeap.triangle(2, 3, 4), Leaf)
    LeftistHeap.triangle(2, 3, 4).merge(LeftistHeap.just(1)) shouldBe
      Node(1, 1, LeftistHeap.triangle(2, 3, 4), Leaf)
    
    Node(1, triangle(2, 3, 4), just(5)).merge(triangle(6, 7, 8)) shouldBe
      Node(2, 1, triangle(2, 3, 4), Node(1, 5, triangle(6, 7, 8), Leaf))
    triangle(6, 7, 8).merge(Node(1, triangle(2, 3, 4), just(5))) shouldBe
      Node(2, 1, triangle(2, 3, 4), Node(1, 5, triangle(6, 7, 8), Leaf))
      
  }
end LeftistHeapSpec

class LeftistHeapCompanionSpec extends AnyFunSuite with Matchers:
  test("#empty") {
    LeftistHeap.empty shouldBe Leaf
  }
  
  test("#just") {
    LeftistHeap.just(1) shouldBe Node(1, 1, Leaf, Leaf)
  }
  
  test("#triangle") {
    triangle(1, 2, 3) shouldBe Node(2, 1, just(2), just(3))
    triangle(2, 3, 1) shouldBe Node(2, 1, just(2), just(3))
    triangle(3, 2, 1) shouldBe Node(2, 1, just(2), just(3))
  }
end LeftistHeapCompanionSpec

class NodeCompanionSpec extends AnyFunSuite with Matchers:
  test("#apply") {
    Node(1, LeftistHeap.just(2), Leaf) shouldBe Node(1, 1, LeftistHeap.just(2), Leaf)
    Node(1, Leaf, LeftistHeap.just(2)) shouldBe Node(1, 1, LeftistHeap.just(2), Leaf)
  }
end NodeCompanionSpec
