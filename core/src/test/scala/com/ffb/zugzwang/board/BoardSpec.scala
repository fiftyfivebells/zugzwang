package com.ffb.zugzwang.board

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.ffb.zugzwang.chess.{Color, Piece, PieceType, Square}

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
    val a1    = Square.from(7, 0).right.get
    val e1    = Square.from(4, 0).right.get
    val e4    = Square.from(4, 3).right.get

    board.pieceAt(e1) shouldBe Some(Piece(Color.White, PieceType.King))
    board.pieceAt(a1) shouldBe Some(Piece(Color.White, PieceType.Rook))
    board.pieceAt(e4) shouldBe None
  }

  ".putPieceAt(square)" should "add a piece at the given square" in {
    val board = Board.empty

    Square.fromAlgebraic("a1") map { sq =>
      val pawn    = Piece(Color.White, PieceType.Pawn)
      val updated = board.putPieceAt(pawn, sq)
      updated == Board.empty shouldBe false
      updated.pieceAt(sq) shouldBe Some(pawn)
    }

    Square.fromAlgebraic("e4") map { sq =>
      val king    = Piece(Color.Black, PieceType.King)
      val updated = board.putPieceAt(king, sq)
      updated == Board.empty shouldBe false
      updated.pieceAt(sq) shouldBe Some(king)
    }
  }

  ".removePieceFrom(square)" should "remove the piece from the given square" in {
    val board = Board.initial

    Square.fromAlgebraic("e1") map { sq =>
      val updated = board.removePieceFrom(sq)
      updated.pieceAt(sq) shouldBe None
    }
  }
