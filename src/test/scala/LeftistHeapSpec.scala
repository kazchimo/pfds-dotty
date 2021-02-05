package pfds

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import LeftistHeap._

class LeftistHeapSpec extends AnyFunSuite with Matchers:
end LeftistHeapSpec

class LeftistHeapCompanionSpec extends AnyFunSuite with Matchers:
  test("#just") {
    LeftistHeap.just(1) shouldBe Node(1, 1, Leaf, Leaf)
  }
end LeftistHeapCompanionSpec

class NodeCompanionSpec extends AnyFunSuite with Matchers:
  test("#apply") {
    Node(1, Node(2, Leaf, Leaf), Leaf) shouldBe Node(1, 1, Node(1, 2, Leaf, Leaf), Leaf)
  }
end NodeCompanionSpec
