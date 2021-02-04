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
    Tree.just(0).insert(1) shouldBe Tree.withRight(0, Tree.just(1))
    Tree.just(0).insert(0) shouldBe Tree.just(0)
    Tree.just(0).insert(-1) shouldBe Tree.withLeft(Tree.just(-1), 0)
  }
end TreeSpec

class TreeCompanionSpec extends AnyFunSuite with Matchers:
  test("#just") {
    Tree.just(1) shouldBe Tree.just(1)
  }
  
  test("#complete") {
    Tree.complete(1, 2) shouldBe same(1)
    Tree.complete(1, 0) shouldBe Leaf
    the[Exception] thrownBy Tree.complete(1, -1)
  }
end TreeCompanionSpec

class NodeCompanionSpec extends AnyFunSuite with Matchers:
  test("#withLeft") {
    Tree.withLeft(Tree.just(1), 2) shouldBe Node(Tree.just(1), 2, Leaf)
  }
  
  test("#withRight") {
    Tree.withRight(1, Tree.just(2)) shouldBe Tree.withRight(1, Tree.just(2))
  }
end NodeCompanionSpec
