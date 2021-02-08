package set

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import RedBlackSet._
import Color._

class RedBlackSetSpec extends AnyFunSuite with Matchers: 
  test("#balance") {
    RedBlackSet.balance(Black, withLeft(Red, justRed(1), 2), 3, Empty) shouldBe
      red(justBlack(1), 2, justBlack(3))
    RedBlackSet.balance(Black, withRight(Red, 1, justRed(2)), 3, Empty) shouldBe
      red(justBlack(1), 2, justBlack(3))
    RedBlackSet.balance(Black, Empty, 1, withLeft(Red, justRed(2), 3)) shouldBe
      red(justBlack(1), 2, justBlack(3))
    RedBlackSet.balance(Black, Empty, 1, withRight(Red, 2, justRed(3))) shouldBe
      red(justBlack(1), 2, justBlack(3))
    RedBlackSet.balance(Red, justBlack(1), 2, justBlack(3)) shouldBe
      red(justBlack(1), 2, justBlack(3))
    RedBlackSet.balance(Black, justRed(1), 2, justRed(3)) shouldBe
      black(justRed(1), 2, justRed(3))
  }

