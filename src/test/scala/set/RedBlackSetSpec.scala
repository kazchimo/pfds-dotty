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
  
  test("#member") {
    black(justRed(1), 3, justRed(5)).member(1) shouldBe true
    black(justRed(1), 3, justRed(5)).member(4) shouldBe false
  }

  test("#insert") {
    black(justRed(1), 3, justRed(5)).insert(2) shouldBe 
      black(justBlack(1), 2, withRight(Black, 3, justRed(5)))
    black(justRed(1), 3, justRed(5)).insert(4) shouldBe 
      black(withLeft(Black, justRed(1), 3), 4, justBlack(5))
    
    red(black(justRed(1), 3, justRed(5)), 7, black(justRed(9), 11, justRed(13))).insert(4) shouldBe
      black(
        red(withLeft(Black, justRed(1), 3), 4, justBlack(5)),
        7,
        black(justRed(9), 11, justRed(13))
      )
  }

