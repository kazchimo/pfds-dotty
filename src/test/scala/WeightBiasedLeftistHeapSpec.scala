package pfds

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import WeightBiasedLeftistHeap._

class WeightBiasedLeftistHeapSpec extends AnyFunSuite with Matchers:
  test("#weight") {
    Leaf.weight shouldBe 0
    just(1).weight shouldBe 1
    triangle(1, 2, 3).weight shouldBe 3
    Node(0, triangle(1, 2, 3), triangle(4, 5, 6)).weight shouldBe 7
  }
  
  test("#merge") {
    just(1).merge(triangle(2, 3, 4)) shouldBe Node(4, 1, triangle(2, 3, 4), Leaf)
    triangle(2, 3, 4).merge(just(1)) shouldBe Node(4, 1, triangle(2, 3, 4), Leaf)
    
    Node(1, triangle(2, 3, 4), just(5)).merge(triangle(6, 7, 8)) shouldBe
      Node(8, 1, Node(4, 5, triangle(6, 7, 8), Leaf), triangle(2, 3, 4))
    triangle(6, 7, 8).merge(Node(1, triangle(2, 3, 4), just(5))) shouldBe
      Node(8, 1, Node(4, 5, triangle(6, 7, 8), Leaf), triangle(2, 3, 4))
  }
  
  test("#insert") {
    WeightBiasedLeftistHeap.empty[Int].insert(1) shouldBe just(1)
    just(1).insert(2) shouldBe Node(2, 1, just(2), Leaf)
    triangle(2, 3, 5).insert(4) shouldBe Node(4, 2, Node(2, 4, just(5), Leaf), just(3))
  }

  test("#isEmpty") {
    Leaf.isEmpty shouldBe true
    just(1).isEmpty shouldBe false
  }

  test("#min") {
    the[Exception] thrownBy Leaf.min
    triangle(3, 2, 1).min shouldBe 1
  }

  test("#deleteMin") {
    the[Exception] thrownBy Leaf.deleteMin
    triangle(2, 3, 1).deleteMin shouldBe Node(2, 2, Node(1, 3, Leaf, Leaf), Leaf)
  }
end WeightBiasedLeftistHeapSpec

class WeightBiasedLeftistHeapCompanionSpec extends AnyFunSuite with Matchers:
  test("#just") {
    just(1) shouldBe Node(1, 1, Leaf, Leaf)
  }

  test("#triangle") {
    triangle(1, 2, 3) shouldBe Node(3, 1, just(2), just(3))
  }

  test("Node#apply") {
    Node(1, Leaf, Leaf) shouldBe Node(1, 1, Leaf, Leaf)
    Node(1, just(3), just(2)) shouldBe Node(3, 1, just(3), just(2))
  }
end WeightBiasedLeftistHeapCompanionSpec

