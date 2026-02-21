package com.ffb.zugzwang.board

import com.ffb.zugzwang.chess.{Piece, Square}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardSpec extends AnyFlatSpec with Matchers:

  ".toFen" should "convert a board into a string representing the fen of the position" in {
    val initial = Board.initial

    val fen             = initial.toFen
    val initialBoardFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

    fen shouldBe initialBoardFen
  }

  "Board.from(fen)" should "correctly add pieces to board based on fen string" in {
    val initialBoardFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

    val board = Board.from(initialBoardFen)

    board.pieceAt(Square.A1) shouldBe Piece.WhiteRook
    board.pieceAt(Square.E1) shouldBe Piece.WhiteKing
    board.pieceAt(Square.E4) shouldBe Piece.NoPiece
  }

  ".putPieceAt(square)" should "add a piece at the given square" in {
    val board = Board.empty

    Square.fromAlgebraic("a1") map { sq =>
      val pawn    = Piece.WhitePawn
      val updated = board.putPieceAt(pawn, sq)
      updated == Board.empty shouldBe false
      updated.pieceAt(sq) shouldBe pawn
    }

    Square.fromAlgebraic("e4") map { sq =>
      val king    = Piece.BlackKing
      val updated = board.putPieceAt(king, sq)
      updated == Board.empty shouldBe false
      updated.pieceAt(sq) shouldBe king
    }
  }

  ".removePieceFrom(square)" should "remove the piece from the given square" in {
    val board = Board.initial

    Square.fromAlgebraic("e1") map { sq =>
      val updated = board.removePieceFrom(sq)
      updated.pieceAt(sq) shouldBe Piece.NoPiece
    }
  }
