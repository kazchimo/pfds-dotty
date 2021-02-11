package queue

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TwoListDeuqueueSpec extends AnyFunSuite with Matchers: 
  test("#isEmpty") {
    TwoListDequeue.empty.isEmpty shouldBe true
    TwoListDequeue(List(1), Nil).isEmpty shouldBe false
    TwoListDequeue(Nil, List(1)).isEmpty shouldBe false
    TwoListDequeue(List(1), List(2)).isEmpty shouldBe false
  }

  test("#+:") {
    1 +: TwoListDequeue.empty shouldBe TwoListDequeue.front(1)
    1 +: TwoListDequeue.front(2) shouldBe TwoListDequeue(1)(2)
    1 +: TwoListDequeue.rear(2) shouldBe TwoListDequeue(1)(2)
    1 +: TwoListDequeue(2)(3) shouldBe TwoListDequeue(1, 2)(3)
  }

  test("#head") {
    the[Exception] thrownBy TwoListDequeue.empty.head
    TwoListDequeue(1)(2, 3).head shouldBe 1
  }
  
  test("#tail") {
    the[Exception] thrownBy TwoListDequeue.empty.tail
    TwoListDequeue(1)(2, 3).tail shouldBe TwoListDequeue(2)(3)
    TwoListDequeue(1, 2)(3).tail shouldBe TwoListDequeue(2)(3)
  }

  test("#:+") {
    TwoListDequeue.empty :+ 1 shouldBe TwoListDequeue.rear(1)
    TwoListDequeue.front(1) :+ 2 shouldBe TwoListDequeue(1)(2)
    TwoListDequeue(1)(2) :+ 3 shouldBe TwoListDequeue(1)(2, 3)
  }

  test("#last") {
    the[Exception] thrownBy TwoListDequeue.empty.last
    TwoListDequeue(1)(2).last shouldBe 2
    TwoListDequeue.front(1).last shouldBe 1
    TwoListDequeue.rear(1).last shouldBe 1
  }

  test("#init") {
    the[Exception] thrownBy TwoListDequeue.empty.init
    TwoListDequeue(1)(2).init shouldBe TwoListDequeue.front(1)
    TwoListDequeue(1, 2)(3).init shouldBe TwoListDequeue(1)(2)
    TwoListDequeue.rear(3).init shouldBe TwoListDequeue.empty
    TwoListDequeue.front(3).init shouldBe TwoListDequeue.empty
  }
