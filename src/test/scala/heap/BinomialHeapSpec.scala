package heap

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BinomialHeapSpec extends AnyFunSuite with Matchers:
  test("#apply") {
    val bh = BinomialHeap(BinomialTree.rank1(2, 3), BinomialTree.rank0(1))
    bh.trees(0) shouldBe BinomialTree.rank0(1)
    bh.trees(1) shouldBe BinomialTree.rank1(2, 3)
  }
  
  test("#insTree") {
    // 1 + 1 = 10 (binary digit)
    BinomialHeap(BinomialTree.rank0(0)).insTree(BinomialTree.rank0(1)) shouldBe
      BinomialHeap(BinomialTree.rank1(0, 1))

    BinomialHeap(BinomialTree.rank1(1, 2)).insTree(BinomialTree.rank0(0)) shouldBe
      BinomialHeap(BinomialTree.rank0(0), BinomialTree.rank1(1, 2))
  }
  
  test("BinomialTree#link") {
    BinomialTree.just(0).link(BinomialTree.just(1)) shouldBe 
      BinomialTree(1, 0, List(BinomialTree.just(1)))
    
    BinomialTree.rank0(4).link(BinomialTree.rank0(3)) shouldBe
      BinomialTree.rank1(3, 4)
    
    BinomialTree.rank1(1, 2).link(BinomialTree.rank1(3, 4)) shouldBe
      BinomialTree.rank2(1, 2, 3, 4)
  }
end BinomialHeapSpec
  
  
