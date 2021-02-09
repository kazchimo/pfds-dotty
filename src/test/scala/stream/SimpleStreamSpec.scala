package stream

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import SimpleStream._

import java.lang.Thread.sleep

class SimpleStreamSpec extends AnyFunSuite with Matchers:
  test("#take") {
    SCons(1, SCons(2, SCons(3, SNil))).take(2).tail.head shouldBe 2
  }

  test("#drop") {
    SCons(1, SCons(2, SCons(3, SNil))).drop(1).head shouldBe 2
  }

  test("reverse") {
    SCons(1, SCons(2, SCons(3, SNil))).reverse.head shouldBe 3
  }

