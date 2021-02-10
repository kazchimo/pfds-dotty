package queue

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BatchedQueueSpec extends AnyFunSuite with Matchers:
  test("#isEmpty") {
    BatchedQueue.empty.isEmpty shouldBe true
    BatchedQueue(List(1), Nil).isEmpty shouldBe false
  }

  test("#:+") {
    BatchedQueue.empty :+ 1 shouldBe BatchedQueue(List(1), Nil)
    BatchedQueue(List(1), Nil) :+ 2 shouldBe BatchedQueue(List(1), List(2))
  }

  test("#head") {
    BatchedQueue(List(1), Nil).head shouldBe 1
    the[Exception] thrownBy BatchedQueue.empty.head
  }

  test("#tail") {
    BatchedQueue(List(1), List(2)).tail shouldBe BatchedQueue(List(2), Nil)
    BatchedQueue(List(1, 2), Nil).tail shouldBe BatchedQueue(List(2), Nil)
    the[Exception] thrownBy BatchedQueue.empty.tail
  }
