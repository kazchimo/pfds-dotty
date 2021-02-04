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
    LinkedList.cons(1, Nil) shouldBe Cons(1, Nil)
  }

  test("#apply") {
    LinkedList(1, 2, 3) shouldBe Cons(1, Cons(2, Cons(3, Nil)))
    LinkedList() shouldBe Nil
  }

class LinkedListSpec extends AnyFunSuite with Matchers:
  test("#isEmpty") {
    Nil.isEmpty shouldBe true
    Cons(1, Nil).isEmpty shouldBe false
  }

  test("#head") {
    the[Exception] thrownBy Nil.head
    Cons(1, Nil).head shouldBe 1
  }

  test("#tail") {
    the[Exception] thrownBy Nil.tail
    Cons(1, Nil).tail shouldBe Nil
  }

  test("#++") {
    Nil ++ Cons(1, Nil) shouldBe Cons(1, Nil)
    Cons(1, Nil) ++ Nil shouldBe Cons(1, Nil)
    Cons(1, Nil) ++ Cons(2, Nil) shouldBe Cons(1, Cons(2, Nil))
  }
  

