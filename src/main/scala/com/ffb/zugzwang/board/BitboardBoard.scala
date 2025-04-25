package com.ffb.zugzwang.board

import scala.collection.immutable.ArraySeq
import com.ffb.zugzwang.chess.{Color, Piece, Square}
import com.ffb.zugzwang.chess.PieceType
import scala.collection.mutable

enum PieceCategory:
  case WP, WN, WB, WR, WQ, WK, BP, BN, BB, BR, BQ, BK

object PieceCategory:
  def of(piece: Piece): PieceCategory = piece match {
    case Piece(Color.White, PieceType.Pawn)   => WP
    case Piece(Color.White, PieceType.Knight) => WN
    case Piece(Color.White, PieceType.Bishop) => WB
    case Piece(Color.White, PieceType.Rook)   => WR
    case Piece(Color.White, PieceType.Queen)  => WQ
    case Piece(Color.White, PieceType.King)   => WK
    case Piece(Color.Black, PieceType.Pawn)   => BP
    case Piece(Color.Black, PieceType.Knight) => BN
    case Piece(Color.Black, PieceType.Bishop) => BB
    case Piece(Color.Black, PieceType.Rook)   => BR
    case Piece(Color.Black, PieceType.Queen)  => BQ
    case Piece(Color.Black, PieceType.King)   => BK
  }
end PieceCategory

final case class BitboardBoard private (
    pieces: IArray[Bitboard],
    squares: ArraySeq[Option[Piece]]
) extends Board:

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

  def clearBoard: Board = BitboardBoard.empty

  def pieceAt(sq: Square): Option[Piece] = squares(sq.value)

  def putPieceAt(p: Piece, sq: Square): Board =
    val pieceIndex = PieceCategory.of(p).ordinal
    val newBitboard = pieces(pieceIndex).setBitAt(sq)

    BitboardBoard(
      pieces.updated(pieceIndex, newBitboard),
      squares.updated(sq.value, Some(p))
    )

  def removePieceFrom(sq: Square): Board =
    squares(sq.value) match {
      case None => this
      case Some(piece) =>
        val pieceIndex = PieceCategory.of(piece).ordinal
        val newBitboard = pieces(pieceIndex).clearBitAt(sq)
        val newSquares = squares.updated(sq.value, None)

        BitboardBoard(pieces.updated(pieceIndex, newBitboard), newSquares)
    }

  def isKingAttacked(c: Color): Boolean =
    val kingIndex =
      if c == Color.White then PieceCategory.WK else PieceCategory.BK

    val kingSquare = pieces(kingIndex.ordinal).leastSignificantBit

    kingSquare match
      case Some(square) => isAttacked(square, c)

      // if we can't find the king, then there's a bigger problem, but since that's unlikely
      // I'm just going to mark it as false
      case None => false

  def isAttacked(sq: Square, c: Color): Boolean = ???

  def isAttackedByPawn(sq: Square, c: Color): Boolean = ???

  private def pieceToCategory(p: Piece): PieceCategory = p match
    case Piece(Color.White, PieceType.Pawn)   => PieceCategory.WP
    case Piece(Color.White, PieceType.Knight) => PieceCategory.WN

end BitboardBoard

object BitboardBoard:

  def empty: Board =
    BitboardBoard(IArray.fill(12)(Bitboard.empty), ArraySeq.fill(64)(None))

  def initial: Board =
    BitboardBoard.from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

  def from(fen: String): Board =
    val fenNoDashes = fen.replaceAll("/", "")
    val fenNoNumbers =
      fenNoDashes
        .map(ch => if ch.isDigit then "*" * ch.asDigit else ch.toString)
        .mkString

    fenNoNumbers.zipWithIndex.foldLeft(BitboardBoard.empty) { (bb, pair) =>
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

end BitboardBoard
