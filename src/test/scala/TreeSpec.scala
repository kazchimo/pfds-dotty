package pfds

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import Tree._

class TreeSpec extends AnyFunSuite with Matchers:  
  test("#member") {
    Leaf.member(1) shouldBe false
    Node(Leaf, 1, Leaf).member(1) shouldBe true
    Node(Leaf, 0, Leaf).member(1) shouldBe false
  }
  
  test("#insert") {
    Leaf.insert(1) shouldBe Node(Leaf, 1, Leaf)
    Node(Leaf, 0, Leaf).insert(1) shouldBe Node(Leaf, 0, Node(Leaf, 1, Leaf))
    Node(Leaf, 0, Leaf).insert(-1) shouldBe Node(Node(Leaf, -1, Leaf), 0, Leaf)
  }
