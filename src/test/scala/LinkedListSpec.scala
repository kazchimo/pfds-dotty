package pfds
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import LinkedList._

class LinkedListSpec extends AnyFunSuite with Matchers:
  test("#empty") {
    LinkedList.empty[Int] shouldBe LinkedList.Nil
  }

  test("#isEmpty") {
    Nil.isEmpty shouldBe true
    Cons(1, Nil).isEmpty shouldBe false
  }
  

