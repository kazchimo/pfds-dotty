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

