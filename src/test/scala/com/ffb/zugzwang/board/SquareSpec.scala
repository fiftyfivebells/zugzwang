package com.ffb.zugzwang.board

import org.scalatest.funsuite.AnyFunSuite
import com.ffb.zugzwang.chess.Square
import com.ffb.zugzwang.chess.File

class SquareSpec extends AnyFunSuite:

  test(
    "fromAlgebraic should convert 'h1' to a valid Square which toAlgebraic returns 'h1'"
  ):
    val result = Square.fromAlgebraic("h1")
    assert(result.isRight, "Expected Right for input 'h1'")
    result match
      case Right(square) =>
        val alg = Square.toAlgebraic(square)
        assert(alg == "h1", s"toAlgebraic returned '$alg', expected 'h1'")

      case Left(error) =>
        fail(s"Unexpected error for 'h1': $error")

  test("toAlgebraic should convert a Square created with from(7, 0) to 'h1'"):
    val squareResult = Square.from(7, 0)
    assert(squareResult.isRight, "Square.from(0, 0) should succeed")
    squareResult match
      case Right(square) =>
        val alg = Square.toAlgebraic(square)
        assert(alg == "h1", s"Expected 'h1' but got '$alg'")

      case Left(err) =>
        fail(s"Unexpected error in smart constructor: $err")

  test(
    "fromAlgebraic should convert 'a8' to a valid Square which toAlgebraic returns 'a8'"
  ):
    val result = Square.fromAlgebraic("a8")
    assert(result.isRight, "Expected Right for input 'a8'")
    result match
      case Right(square) =>
        val alg = Square.toAlgebraic(square)
        assert(alg == "a8", s"Expected 'a8' but got '$alg'")

      case Left(error) =>
        fail(s"Unexpected error for 'a8': $error")

  test("fromAlgebraic should return Left for invalid inputs"):
    val inputs = List("z9", "a0", "11", "abc", "")
    inputs.foreach { input =>
      val result = Square.fromAlgebraic(input)
      assert(result.isLeft, s"Expected Left for input '$input'")
    }

  test(
    "Smart constructor from(file, rank) should return Left for invalid coordinates"
  ):
    assert(Square.from(-1, 0).isLeft, "Expected failure for file = -1")
    assert(Square.from(0, 8).isLeft, "Expected failure for rank = 8")
    assert(
      Square.from(8, 8).isLeft,
      "Expected failure for file and rank out of range"
    )

  test(
    "Smart constructor from(file, rank) should succeed for valid coordinates"
  ):
    val result = Square.from(4, 3)
    assert(result.isRight, "Expected success for coordinates (0, 0)")
    result match
      case Right(square) =>
        val alg = Square.toAlgebraic(square)
        assert(alg == "e4", s"Expected 'e4' for (4, 3) but got '$alg'")

      case Left(err) =>
        fail(s"Unexpected error for valid coordinates: $err")
