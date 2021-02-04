package pfds
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinkedListSpec extends AnyFlatSpec with Matchers:
  it should "empty returns Nil" in {
    LinkedList.empty[Int] shouldBe LinkedList.Nil
  }
  

