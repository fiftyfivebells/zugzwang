package com.ffb.zugzwang.move

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.ffb.zugzwang.chess.Square

class MoveSpec extends AnyFlatSpec with Matchers:

  val testMove = Move(Square.E2, Square.E4, None, MoveType.Quiet)

  ".from" should "return the square e2 for testMove" in {
    val alg = Square.toAlgebraic(testMove.from)
    alg shouldBe "e2"
  }

  ".to" should "return the square e4 for testMove" in {
    val alg = Square.toAlgebraic(testMove.to)
    alg shouldBe "e4"
  }

  ".promotion" should "return None for testMove" in {
    val promo = testMove.promotion
    promo shouldBe None
  }

  ".moveType" should "return MoveType.Quiet for testMove" in {
    val mt = testMove.moveType
    mt shouldBe MoveType.Quiet
  }
