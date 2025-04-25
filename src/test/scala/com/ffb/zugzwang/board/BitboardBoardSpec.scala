package com.ffb.zugzwang.board

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.ffb.zugzwang.chess.{Color, Piece, PieceType, Square}

class BitboardBoardSpec extends AnyFlatSpec with Matchers:

  "BitboardBoard.from(fen)" should "correctly add pieces to board based on fen string" in {
    val initialBoardFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

    val bbBoard = BitboardBoard.from(initialBoardFen)
    val a1 = Square.from(7, 0).right.get
    val e1 = Square.from(4, 0).right.get
    val e4 = Square.from(4, 3).right.get

    bbBoard.pieceAt(e1) shouldBe Some(Piece(Color.White, PieceType.King))
    bbBoard.pieceAt(a1) shouldBe Some(Piece(Color.White, PieceType.Rook))
    bbBoard.pieceAt(e4) shouldBe None
  }

  ".putPieceAt(square)" should "add a piece at the given square" in {
    val bbBoard = BitboardBoard.empty

    Square.fromAlgebraic("a1") map { sq =>
      val pawn = Piece(Color.White, PieceType.Pawn)
      val updated = bbBoard.putPieceAt(pawn, sq)
      updated == BitboardBoard.empty shouldBe false
      updated.pieceAt(sq) shouldBe Some(pawn)
    }

    Square.fromAlgebraic("e4") map { sq =>
      val king = Piece(Color.Black, PieceType.King)
      val updated = bbBoard.putPieceAt(king, sq)
      updated == BitboardBoard.empty shouldBe false
      updated.pieceAt(sq) shouldBe Some(king)
    }
  }

  ".removePieceFrom(square)" should "remove the piece from the given square" in {
    val bbBoard = BitboardBoard.initial

    Square.fromAlgebraic("e1") map { sq =>
      val updated = bbBoard.removePieceFrom(sq)
      updated.pieceAt(sq) shouldBe None
    }
  }
