package pfds

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import Tree._

class TreeSpec extends AnyFunSuite with Matchers:
  test("#member") {
    Leaf.member(1) shouldBe false
    Tree.just(1).member(1) shouldBe true
    Tree.just(0).member(1) shouldBe false
  }

  test("#insert") {
    Leaf.insert(1) shouldBe Tree.just(1)
    Tree.just(0).insert(1) shouldBe Node(Leaf, 0, Tree.just(1))
    Tree.just(0).insert(-1) shouldBe Node.withLeft(Tree.just(-1), 0)
  }

class TreeCompanionSpec extends AnyFunSuite with Matchers:
  test("#just") {
    Tree.just(1) shouldBe Tree.just(1)
  }

class NodeCompanionSpec extends AnyFunSuite with Matchers:
  test("#withLeft") {
    Node.withLeft(Tree.just(1), 2) shouldBe Node(Tree.just(1), 2, Leaf)
  }
  
  test("#withRight") {
    Node.withRight(1, Tree.just(2)) shouldBe Node.withRight(1, Tree.just(2))
  }
