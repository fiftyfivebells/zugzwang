package com.ffb.zugzwang.rules

import com.ffb.zugzwang.board.Board
import com.ffb.zugzwang.chess.{CastleRights, Color, GameState, PieceType, Square}
import com.ffb.zugzwang.move.{Move, MoveGenerator, MoveType}
import com.ffb.zugzwang.notation.FENParser
import com.ffb.zugzwang.rules.Rules
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ZugzwangSpec extends AnyFlatSpec with Matchers:

  "Zugzwang" should "support a single pawn push" in {
    val move       = Move(Square.E2, Square.E3, MoveType.Quiet)
    val movedState = Rules.applyMove(GameState.initial, move)

    val expectedState = GameState(
      Board.from("rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR"),
      Color.Black,
      CastleRights.from("KQkq"),
      None,
      0,
      1
    )

    GameState.sameAs(movedState, expectedState) shouldBe true
  }

  it should "not generate a move that puts the king in check" in {
    val fen = "rnbqkbnr/pppp1ppp/8/1B2p3/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2"

    FENParser.from(fen) match
      case Right(state) =>
        val illegal = Move(Square.D7, Square.D6, MoveType.Quiet)
        MoveGenerator.legalMoves(state).contains(illegal) shouldBe false
      case Left(error) => fail(s"Failed to parse FEN: $error")
  }

  it should "correctly identify stalemate" in {
    val stalemateFen = "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1"

    FENParser.from(stalemateFen) match
      case Right(state) => Rules.isStaleMate(state) shouldBe true
      case Left(error)  => fail(s"Failed to parse FEN: $error")
  }

  it should "correctly identify insufficient material situations" in {
    val kkFen   = "8/8/8/8/8/8/2k5/3K4 w - - 0 1"
    val kkbFen  = "8/8/8/8/8/8/2k5/3KB3 w - - 0 1"
    val kknFen  = "8/8/8/8/8/8/2k5/3KN3 w - - 0 1"
    val kbkbFen = "8/8/8/8/4b3/8/1k6/3K1B2 w - - 0 1"

    val insufficientFens = List(kkFen, kkbFen, kknFen, kbkbFen)

    val allInsufficient = insufficientFens.forall(fen =>
      FENParser.from(fen) match
        case Right(state) =>
          val result = Rules.isInsufficientMaterial(state)
          println(s"$fen: $result")
          result
        case Left(error) =>
          fail(s"Failed to parse FEN $fen: $error")
    )

    allInsufficient shouldBe true
  }

  it should "correctly identify a draw by 50 move rule" in {
    val fen = "8/8/8/8/8/8/2k5/3K4 w - - 100 101"

    FENParser.from(fen) match
      case Right(state) => Rules.isDraw(state) shouldBe true
      case Left(error)  => fail(s"Failed to parse FEN: $error")
  }

  it should "correctly generate 4 promotions and no other moves" in {
    val fen = "8/5P2/8/8/8/8/8/7k w - - 0 1"

    FENParser.from(fen) match
      case Right(state) =>
        val promotions = MoveGenerator.legalMoves(state)

        promotions.length shouldBe 4

        val promotionTypes =
          Set(PieceType.Knight, PieceType.Bishop, PieceType.Rook, PieceType.Queen)

        promotions.collect { case Move(_, _, p, _) =>
          p
        }.toSet
          .intersect(promotionTypes)
          .size shouldBe 4
      case Left(error) => fail(s"Failed to parse FEN: $error")
  }
