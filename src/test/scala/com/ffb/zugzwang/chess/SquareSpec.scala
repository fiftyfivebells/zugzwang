package com.ffb.zugzwang.board

import com.ffb.zugzwang.chess.{File, Square}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SquareSpec extends AnyFlatSpec with Matchers:

  // TODO: these next three tests are very similar; I should figure out a way to DRY them up
  "Square.fromAlgebraic" should "convert 'h1' to a valid Square which toAlgebraic converts back to 'h1'" in {
    val result = Square.fromAlgebraic("h1")

    result.isRight shouldBe true
    result match
      case Right(square) =>
        val alg = Square.toAlgebraic(square)
        square.value shouldBe 0
        alg shouldBe "h1"

      case Left(error) =>
        fail(s"Unexpected error for 'h1': $error")
  }

  "Square.fromAlgebraic" should "convert 'a8' to a valid Square with toAlgebraic converts back to 'a8'" in {
    val result = Square.fromAlgebraic("a8")

    result.isRight shouldBe true
    result match
      case Right(square) =>
        val alg = Square.toAlgebraic(square)
        square.value shouldBe 63
        alg shouldBe "a8"

      case Left(error) =>
        fail(s"Unexpected error for 'a8': $error")
  }

  "Square.fromAlgebraic" should "convert 'a1' to a valid Square which toAlgebraic converts back to 'a1'" in {
    val result = Square.fromAlgebraic("a1")

    result.isRight shouldBe true
    result match
      case Right(square) =>
        val alg = Square.toAlgebraic(square)
        square.value shouldBe 7
        alg shouldBe "a1"

      case Left(error) =>
        fail(s"Unexpected error for 'a1': $error")
  }

  "Square.toAlgebraic" should "convert a Square created with from(7, 0) to 'h1'" in {
    val squareResult = Square.from(7, 0)

    squareResult.isRight shouldBe true
    squareResult match
      case Right(square) =>
        val alg = Square.toAlgebraic(square)
        alg shouldBe "h1"

      case Left(err) =>
        fail(s"Unexpected error in smart constructor: $err")
  }

  "Square.fromAlgebraic" should "return Left for invalid inputs" in {
    val inputs = List("z9", "a0", "11", "abc", "")
    inputs.foreach { input =>
      val result = Square.fromAlgebraic(input)
      result.isLeft shouldBe true
    }
  }

  "Square.from(file, rank)" should "return Left for invalid coordinates" in {
    assert(Square.from(-1, 0).isLeft, "Expected failure for file = -1")
    assert(Square.from(0, 8).isLeft, "Expected failure for rank = 8")
    assert(
      Square.from(8, 8).isLeft,
      "Expected failure for file and rank out of range"
    )
  }

  "Square.from(file, rank)" should "succeed for valid coordinates" in {
    val result = Square.from(4, 3)
    result.isRight shouldBe true
    result match
      case Right(square) =>
        val alg = Square.toAlgebraic(square)
        assert(alg == "e4", s"Expected 'e4' for (4, 3) but got '$alg'")

      case Left(err) =>
        fail(s"Unexpected error for valid coordinates: $err")
  }

  "Square.from(63)" should "create a Square that converts to 'a8' using Square.toAlgebraic" in {
    val squareOpt = Square.from(63)
    assert(squareOpt.nonEmpty)

    squareOpt map { square =>
      val alg = Square.toAlgebraic(square)
      alg shouldBe "a8"
    }
  }

  "Square.from(0)" should "create a Square that converts to 'h1' using Square.toAlgebraic" in {
    val squareOpt = Square.from(0)
    assert(squareOpt.nonEmpty)

    squareOpt map { square =>
      val alg = Square.toAlgebraic(square)
      alg shouldBe "h1"
    }
  }
