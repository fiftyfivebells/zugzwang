package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.{Piece, Square}
import com.ffb.zugzwang.core.Score

sealed trait HistoryTable:
  protected inline val PieceTypeCount = 6
  protected inline val PieceCount     = 12
  protected inline val SquareCount    = 64
  protected inline def TableSize: Int

  protected inline def updateScore(table: Array[Score], index: Int, bonus: Score): Unit =
    val current = table(index)
    table(index) = gravity(current, bonus)

  private inline def gravity(current: Score, bonus: Score): Score =
    current + bonus - current * bonus.abs / Score(SearchConfig.historyMax)

object QuietHistory extends HistoryTable:
  opaque type Table = Array[Score]

  protected inline val TableSize = PieceCount * SquareCount // pieces * squares

  def empty: Table = new Array[Score](TableSize)

  extension (qht: Table)
    inline def index(piece: Piece, to: Square): Int = piece * 64 + to.toInt

    inline def add(piece: Piece, to: Square, bonus: Score): Unit =
      val i = index(piece, to)
      updateScore(qht, i, bonus)

    inline def get(piece: Piece, to: Square): Score =
      val i = index(piece, to)
      qht(i)

    def clear(): Unit =
      java.util.Arrays.fill(qht.asInstanceOf[Array[Int]], 0)

object CaptureHistory extends HistoryTable:
  opaque type Table = Array[Score]

  // color * pieces * squares * piece = color * moving type * to square * captured type
  protected inline val TableSize = 2 * PieceTypeCount * SquareCount * PieceTypeCount

  def empty: Table = new Array[Score](TableSize)

  extension (cht: Table)
    inline def index(moving: Piece, captured: Piece, to: Square): Int =
      val color = moving.color.ordinal

      color * (PieceTypeCount * SquareCount * PieceTypeCount) +
        moving.pieceType * (SquareCount * PieceTypeCount) +
        captured.pieceType

    inline def add(moving: Piece, captured: Piece, to: Square, bonus: Score): Unit =
      val i = index(moving, captured, to)
      updateScore(cht, i, bonus)

    inline def get(moving: Piece, captured: Piece, to: Square): Score =
      val i = index(moving, captured, to)
      cht(i)

    inline def clear(): Unit =
      java.util.Arrays.fill(cht.asInstanceOf[Array[Int]], 0)

object ContinuationHistory extends HistoryTable:
  opaque type Table = Array[Score]

  private inline val ToStride        = 1
  private inline val PieceStride     = SquareCount * ToStride
  private inline val PrevToStride    = PieceTypeCount * PieceStride
  private inline val PrevPieceStride = SquareCount * PrevToStride
  private inline val ColorStride     = PieceTypeCount * PrevPieceStride

  protected inline val TableSize = 2 * ColorStride

  def empty: Table = new Array[Score](TableSize)

  extension (cont: Table)
    inline def index(prevPiece: Piece, piece: Piece, prevTo: Square, to: Square): Int =
      val color = prevPiece.color.ordinal

      color * ColorStride +
        prevPiece.pieceType * PrevPieceStride +
        prevTo.toInt * PrevToStride +
        piece.pieceType * PieceStride +
        to.toInt * ToStride

    inline def add(prevPiece: Piece, piece: Piece, prevTo: Square, to: Square, bonus: Score): Unit =
      val i = index(prevPiece, piece, prevTo, to)
      updateScore(cont, i, bonus)

    inline def get(prevPiece: Piece, piece: Piece, prevTo: Square, to: Square): Score =
      val i = index(prevPiece, piece, prevTo, to)
      cont(i)

    inline def clear(): Unit =
      java.util.Arrays.fill(cont.asInstanceOf[Array[Int]], 0)
