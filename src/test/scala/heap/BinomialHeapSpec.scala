package heap

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BinomialHeapSpec extends AnyFunSuite with Matchers:
  test("BinomialTree.link") {
    BinomialTree.just(0).link(BinomialTree.just(1)) shouldBe 
      BinomialTree(1, 0, List(BinomialTree.just(1)))
    
    BinomialTree.rank0(4).link(BinomialTree.rank0(3)) shouldBe
      BinomialTree.rank1(3, 4)
    
    BinomialTree.rank1(1, 2).link(BinomialTree.rank1(3, 4)) shouldBe
      BinomialTree.rank2(1, 2, 3, 4)
  }
  
  
