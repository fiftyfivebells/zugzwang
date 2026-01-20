package com.ffb.zugzwang.notation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FENParserSpec extends AnyFlatSpec with Matchers:

  "FENParser" should "accept the initial position and return a GameState" in {
    val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    FENParser.from(fen).isRight shouldBe true
  }

  it should "fail if fewer than 6 fields are provided" in {
    val fen    = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w"
    val parsed = FENParser.from(fen)

    parsed.isLeft shouldBe true
    assertResult(FENParserError.MalformedInput(fen))(parsed.left.get)
  }

  it should "fail if board has incorrect number of ranks" in {
    val fen    = "8/8/8/8/8/8/8 w KQkq - 0 1"
    val parsed = FENParser.from(fen)

    val boardStr = fen.split(" ")(0)

    parsed.isLeft shouldBe true
    assertResult(FENParserError.BoardMissingRanks(boardStr))(parsed.left.get)
  }

  it should "fail if a rank sums to more than 8 squares" in {
    val fen    = "9/8/8/8/8/8/8/8 w KQkq - 0 1"
    val parsed = FENParser.from(fen)

    val boardStr = fen.split(" ")(0)

    parsed.isLeft shouldBe true
    assertResult(FENParserError.BoardRankMalformed(boardStr))(parsed.left.get)
  }

  it should "fail if a rank contains invalid characters" in {
    val fen    = "1t6/8/8/8/8/8/8/8 w KQkq - 0 1"
    val parsed = FENParser.from(fen)

    val boardStr = fen.split(" ")(0)

    parsed.isLeft shouldBe true
    assertResult(FENParserError.BoardInvalidChars(boardStr))(parsed.left.get)
  }

  it should "fail if active color is invalid" in {
    val fen    = "8/8/8/8/8/8/8/8 x KQkq - 0 1"
    val parsed = FENParser.from(fen)

    val sideStr = fen.split(" ")(1)

    parsed.isLeft shouldBe true
    assertResult(FENParserError.InvalidActiveSide(sideStr))(parsed.left.get)
  }

  it should "fail if castling rights have too many characters" in {
    val fen    = "8/8/8/8/8/8/8/8 w KQkqKQkq - 0 1"
    val parsed = FENParser.from(fen)

    val crStr = fen.split(" ")(2)

    parsed.isLeft shouldBe true
    assertResult(FENParserError.CastleRightsTooLong(crStr))(parsed.left.get)
  }

  it should "fail if castling rights have invalid char" in {
    val fen    = "8/8/8/8/8/8/8/8 w KXkz - 0 1"
    val parsed = FENParser.from(fen)

    val crStr = fen.split(" ")(2)

    parsed.isLeft shouldBe true
    assertResult(FENParserError.CastleRightsInvalidChar(crStr))(parsed.left.get)
  }

  it should "fail if en passant square is invalid" in {
    val fen    = "8/8/8/8/8/8/8/8 w - z9 0 1"
    val parsed = FENParser.from(fen)

    val epStr = fen.split(" ")(3)

    parsed.isLeft shouldBe true
    assertResult(FENParserError.EPSquareInvalid(epStr))(parsed.left.get)
  }

  it should "fail if en passant is not on rank 3 or 6" in {
    val fen = "8/8/8/8/8/8/8/8 w - e4 0 1"
    FENParser.from(fen).isLeft shouldBe true
  }

  it should "fail if halfmove clock is not a valid integer" in {
    val fen    = "8/8/8/8/8/8/8/8 w - - x 1"
    val parsed = FENParser.from(fen)

    val hmStr = fen.split(" ")(4)

    parsed.isLeft shouldBe true
    assertResult(FENParserError.BadMoveCount("half move", hmStr))(
      parsed.left.get
    )
  }

  it should "fail if fullmove number is not a valid integer" in {
    val fen    = "8/8/8/8/8/8/8/8 w - - 0 x"
    val parsed = FENParser.from(fen)

    val fmStr = fen.split(" ")(5)

    parsed.isLeft shouldBe true
    assertResult(FENParserError.BadMoveCount("full move", fmStr))(
      parsed.left.get
    )
  }

  it should "fail if a pawn is on the 1st or 8th rank" in {
    val fen = "8/8/8/8/8/8/8/P7 w - - 0 1"
    FENParser.from(fen).isLeft shouldBe true
  }

end FENParserSpec
