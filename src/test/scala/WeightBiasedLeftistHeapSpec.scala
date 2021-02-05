package pfds

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import WeightBiasedLeftistHeap._

class WeightBiasedLeftistHeapSpec extends AnyFunSuite with Matchers:
end WeightBiasedLeftistHeapSpec

class WeightBiasedLeftistHeapCompanionSpec extends AnyFunSuite with Matchers:
  test("#just") {
    just(1) shouldBe Node(1, 1, Leaf, Leaf)
  }

  test("Node#apply") {
    Node(1, Leaf, Leaf) shouldBe Node(1, 1, Leaf, Leaf)
    Node(1, just(3), just(2)) shouldBe Node(3, 1, just(3), just(2))
  }
end WeightBiasedLeftistHeapCompanionSpec

