package pfds
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import LinkedList._

class LinkedListCompanionSpec extends AnyFunSuite with Matchers:
  test("#empty") {
    LinkedList.empty[Int] shouldBe LinkedList.Nil
  }

  test("#cons") {
    LinkedList.cons(1, Nil) shouldBe LinkedList(1)
  }

  test("#apply") {
    LinkedList(1, 2, 3) shouldBe LinkedList(1, 2, 3)
    LinkedList() shouldBe Nil
  }

class LinkedListSpec extends AnyFunSuite with Matchers:
  test("#isEmpty") {
    Nil.isEmpty shouldBe true
    LinkedList(1).isEmpty shouldBe false
  }

  test("#head") {
    the[Exception] thrownBy Nil.head
    LinkedList(1).head shouldBe 1
  }

  test("#tail") {
    the[Exception] thrownBy Nil.tail
    LinkedList(1).tail shouldBe Nil
  }

  test("#++") {
    Nil ++ LinkedList(1) shouldBe LinkedList(1)
    LinkedList(1) ++ Nil shouldBe LinkedList(1)
    LinkedList(1) ++ LinkedList(2) shouldBe LinkedList(1, 2)
  }
  

