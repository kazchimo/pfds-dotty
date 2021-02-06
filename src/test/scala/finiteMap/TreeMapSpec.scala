package finiteMap

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import TreeMap._

class TreeMapSpec extends AnyFunSuite with Matchers:
  test("#bind") {
    Leaf.bind("a", 1) shouldBe just("a", 1)
    just("a", 1).bind("b", 2) shouldBe Node(Leaf, "a", 1, just("b", 2))
    Node(Leaf, "a", 1, just("b", 2)).bind("c", 3) shouldBe Node(Leaf, "a", 1, Node(Leaf, "b", 2, just("c", 3)))
  }

  test("#lookup") {
    the[Exception] thrownBy Leaf.lookup("a")
    just("a", 1).lookup("a") shouldBe 1
    the[Exception] thrownBy just("a", 1).lookup("b")
    Node(Leaf, "a", 1, just("b", 2)).lookup("b") shouldBe 2
  }
