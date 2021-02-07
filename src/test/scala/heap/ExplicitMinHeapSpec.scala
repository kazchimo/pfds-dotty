package heap

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ExplicitMinHeap._

class ExplicitMinHeapSpec extends AnyFunSuite with Matchers:
  test("#min") {
    the[Exception] thrownBy Empty[Nothing, BinomialHeap](BinomialHeap.empty).min
    ExplicitMinHeap[Int, BinomialHeap](
      BinomialHeap(BinomialTree.rank1(1, 2), BinomialTree.rank0(0))
    ).min shouldBe 0
  }
  
  test("#isEmpty") {
    ExplicitMinHeap[Int, BinomialHeap](BinomialHeap.empty).isEmpty shouldBe true
    ExplicitMinHeap[Int, BinomialHeap](
      BinomialHeap(BinomialTree.rank0(0))
    ).isEmpty shouldBe false
  }
  
  test("#insert") {
    ExplicitMinHeap[Int, BinomialHeap](
      BinomialHeap(BinomialTree.rank0(0)) 
    ).insert(1) shouldBe ExplicitMinHeap[Int, BinomialHeap](
      BinomialHeap(BinomialTree.rank1(0, 1)))

    ExplicitMinHeap[Int, BinomialHeap](
      BinomialHeap(BinomialTree.rank1(1, 2))).insert(0) shouldBe
      ExplicitMinHeap[Int, BinomialHeap](BinomialHeap(BinomialTree.rank0(0), BinomialTree.rank1(1, 2)))
  }
  
  test("#merge") {
    ExplicitMinHeap[Int, BinomialHeap](
      BinomialHeap(BinomialTree.rank0(0), BinomialTree.rank1(1, 2))).merge(
      ExplicitMinHeap[Int, BinomialHeap](
        BinomialHeap(BinomialTree.rank2((3, 4), (5, 6))))
    ) shouldBe ExplicitMinHeap[Int, BinomialHeap](BinomialHeap(
      BinomialTree.rank0(0), BinomialTree.rank1(1, 2), BinomialTree.rank2((3, 4), (5, 6))
    ))

    ExplicitMinHeap[Int, BinomialHeap](
      BinomialHeap(BinomialTree.rank0(0), BinomialTree.rank1(1, 2)) 
    ).merge(
      ExplicitMinHeap[Int, BinomialHeap](BinomialHeap(BinomialTree.rank0(3)))
    ) shouldBe ExplicitMinHeap[Int, BinomialHeap]((BinomialHeap(BinomialTree.rank2((0, 3), (1, 2)))))
  }
  
  test("#deleteMin") {
    the[Exception] thrownBy ExplicitMinHeap[Int, BinomialHeap](BinomialHeap.empty).deleteMin
    
    ExplicitMinHeap[Int, BinomialHeap](
      BinomialHeap(BinomialTree.rank0(0), BinomialTree.rank1(1, 2)) 
    ).deleteMin shouldBe ExplicitMinHeap[Int, BinomialHeap](BinomialHeap(BinomialTree.rank1(1, 2)))
    
    ExplicitMinHeap[Int, BinomialHeap](
      BinomialHeap(BinomialTree.rank2((1, 2), (3, 4)))).deleteMin shouldBe
      ExplicitMinHeap[Int, BinomialHeap](BinomialHeap(BinomialTree.rank0(2), BinomialTree.rank1(3, 4)))
    
    ExplicitMinHeap[Int, BinomialHeap](BinomialHeap(
      BinomialTree.rank0(0), BinomialTree.rank1(1, 2), BinomialTree.rank2((3, 4), (-1, 5))
    )).deleteMin shouldBe ExplicitMinHeap[Int, BinomialHeap](
      BinomialHeap(
        BinomialTree.rank1(0, 5), BinomialTree.rank2((3, 4), (1, 2))
      )
    )
  }
end ExplicitMinHeapSpec
