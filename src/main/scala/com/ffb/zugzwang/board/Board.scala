package com.ffb.zugzwang.board

import scala.collection.immutable.ArraySeq
import com.ffb.zugzwang.chess.{Color, Piece, PieceType, Square}
import scala.collection.mutable
import com.ffb.zugzwang.move.{Attacks, Move, MoveType}

final case class Board private (
    pieces: IArray[Bitboard],
    squares: ArraySeq[Option[Piece]]
):

  // TODO: this is not the most "functional" implementation, so maybe I'll revisit this
  // and try to do this in a nicer way later on. it works for now though
  def toFen: String =
    val fenString = new mutable.StringBuilder("")

    for (rank <- 8 to 1 by -1) {
      var emptySquares = 0
      var startingSquare = rank * 8 - 1

      for (i <- startingSquare to startingSquare - 7 by -1) {
        squares(i) match {
          case None => emptySquares += 1
          case Some(p) =>
            if emptySquares > 0 then
              fenString.append(emptySquares.toString)
              emptySquares = 0

            fenString.append(p.toString)
        }
      }

      if emptySquares > 0 then fenString.append(emptySquares.toString)

      fenString.append("/")
    }

    fenString.dropRight(1).toString

  def occupied: Bitboard = pieces.foldLeft(Bitboard.empty)(_ | _)

  def byColor(c: Color): Bitboard =
    Piece.byColor(c).foldLeft(Bitboard.empty) { (bb, pc) =>
      bb | pieces(pc)
    }

  def byColorAndType(c: Color, pt: PieceType): Bitboard =
    val piece = Piece.from(c, pt)
    pieces(piece)

  def allPieces: List[Piece] = squares.flatMap(identity).toList

  def clearBoard: Board = Board.empty

  def pieceAt(sq: Square): Option[Piece] = squares(sq.value)

  def putPieceAt(p: Piece, sq: Square): Board =
    val newBitboard = pieces(p).setBitAt(sq)

    Board(
      pieces.updated(p, newBitboard),
      squares.updated(sq.value, Some(p))
    )

  def removePieceFrom(sq: Square): Board =
    squares(sq.value) match {
      case None => this
      case Some(piece) =>
        val newBitboard = pieces(piece).clearBitAt(sq)
        val newSquares = squares.updated(sq.value, None)

        Board(pieces.updated(piece, newBitboard), newSquares)
    }

  def isKingAttacked(c: Color): Boolean =
    val kingIndex =
      if c == Color.White then Piece.WhiteKing else Piece.BlackKing

    val kingSquare = pieces(kingIndex).leastSignificantBit

    kingSquare match
      case Some(square) => isAttacked(square, c)

      // if we can't find the king, then there's a bigger problem, but since that's unlikely
      // I'm just going to mark it as false
      case None => false

  def isAttacked(sq: Square, c: Color): Boolean =
    val enemy = c.enemy

    Piece.byColor(c).foldLeft(false) { (isAttacked, piece) =>
      val attackMask = Attacks.attacks(piece, sq, this.occupied)

      isAttacked || (attackMask & byColorAndType(
        enemy,
        piece.pieceType
      )).nonEmpty
    }

object Board:

  def empty: Board =
    Board(IArray.fill(12)(Bitboard.empty), ArraySeq.fill(64)(None))

  def initial: Board =
    Board.from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

  def from(fen: String): Board =
    val fenNoDashes = fen.replaceAll("/", "")
    val fenNoNumbers =
      fenNoDashes
        .map(ch => if ch.isDigit then "*" * ch.asDigit else ch.toString)
        .mkString

    fenNoNumbers.zipWithIndex.foldLeft(Board.empty) { (bb, pair) =>
      val (c, i) = pair
      val index = 63 - i
      if c == '*' then bb
      else
        val piece = Piece.from(c)

        Square.from(index) match {
          case Some(square) => bb.putPieceAt(piece, square)
          case None         => bb
        }
    }

  def applyMove(board: Board, move: Move): Board = move.moveType match {
    case MoveType.CastleKingside | MoveType.CastleQueenside =>
      applyCastleMove(board, move)

    case MoveType.EnPassant =>
      val piece = board.pieceAt(move.from).get
      val epSquare = piece.color match {
        case Color.White => Square(move.to.value - 8)
        case Color.Black => Square(move.to.value + 8)
      }

      board
        .removePieceFrom(move.from)
        .removePieceFrom(epSquare)
        .putPieceAt(piece, move.to)

    case _ =>
      (board.pieceAt(move.from), move.promotion) match
        // this should never happen, but if the piece at the from square is None, just
        // return the original board unaltered
        case (None, _) => board

        case (Some(moving), None) =>
          board
            .removePieceFrom(move.to)
            .removePieceFrom(move.from)
            .putPieceAt(moving, move.to)

        case (Some(pawn), Some(promotion)) =>
          val promoPiece = Piece.from(pawn.color, promotion)
          board
            .removePieceFrom(move.to)
            .removePieceFrom(move.from)
            .putPieceAt(promoPiece, move.to)
  }

  private def applyCastleMove(board: Board, move: Move): Board =
    board.pieceAt(move.from) match {
      // as above, this should never happen, but if we try to apply a castle and there's
      // no king at the from square, just return the board and we'll catch the issue later
      case None => board
      case Some(king) =>
        val rook = Piece.from(king.color, PieceType.Rook)

        val (kingTo, rookFrom, rookTo) = move.moveType match {
          case MoveType.CastleKingside =>
            if king.color == Color.White then (Square.G1, Square.H1, Square.F1)
            else (Square.G8, Square.H8, Square.F8)

          case MoveType.CastleQueenside =>
            if king.color == Color.White then (Square.C1, Square.A1, Square.D1)
            else (Square.C8, Square.A8, Square.D8)

          // this is another just in case; this really shouldn't ever happen, I just
          // didn't want warnings about non-exhaustive matches
          case _ => (Square.A1, Square.A1, Square.A1)
        }

        board
          .removePieceFrom(move.from)
          .removePieceFrom(rookFrom)
          .putPieceAt(king, kingTo)
          .putPieceAt(rook, rookTo)
    }

end Board
