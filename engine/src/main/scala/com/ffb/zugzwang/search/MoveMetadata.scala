package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.{MutablePosition, PieceType}
import com.ffb.zugzwang.core.{Ply, Score}
import com.ffb.zugzwang.evaluation.SEE
import com.ffb.zugzwang.move.Move

// Per-move metadata pre-computed during move scoring, carried forward to search time.
// Avoids repeated pieceAt() calls and SEE invocations in the move loop.
final class MoveMetadata(capacity: Int):
  val pieceType: Array[PieceType]    = new Array[PieceType](capacity)
  val capturedType: Array[PieceType] = new Array[PieceType](capacity)
  val historyScore: Array[Score]     = new Array[Score](capacity)
  val flags: Array[Byte]             = new Array[Byte](capacity)

  private inline val FlagCapture: 1   = 1
  private inline val FlagPromotion: 2 = 2
  private inline val FlagQuiet: 4     = 4
  private inline val FlagGoodNoisy: 8 = 8

  inline def isCapture(i: Int): Boolean   = (flags(i) & FlagCapture) != 0
  inline def isPromotion(i: Int): Boolean = (flags(i) & FlagPromotion) != 0
  inline def isQuiet(i: Int): Boolean     = (flags(i) & FlagQuiet) != 0
  inline def isGoodNoisy(i: Int): Boolean = (flags(i) & FlagGoodNoisy) != 0

  def populate(
    i: Int,
    move: Move,
    position: MutablePosition,
    searchHistory: SearchHistory,
    ply: Ply
  ): Unit =
    val piece = position.pieceAt(move.from)
    pieceType(i) = piece.pieceType

    var f: Int = 0
    if move.isCapture then
      f |= FlagCapture
      capturedType(i) = position.pieceAt(move.to).pieceType
      if SEE.seeGE(position, move) then f |= FlagGoodNoisy
    else capturedType(i) = PieceType.NoType

    if move.isPromotion then f |= FlagPromotion
    if !move.isCapture && !move.isPromotion then f |= FlagQuiet

    flags(i) = f.toByte

    historyScore(i) =
      if !move.isCapture && !move.isPromotion then searchHistory.quietMoveScore(ply, move, piece)
      else Score.Zero

  def swap(i: Int, j: Int): Unit =
    var tmpPt: PieceType = PieceType.NoType
    var tmpScore: Score  = Score.Zero
    var tmpByte: Byte    = 0

    tmpPt = pieceType(i); pieceType(i) = pieceType(j); pieceType(j) = tmpPt
    tmpPt = capturedType(i); capturedType(i) = capturedType(j); capturedType(j) = tmpPt
    tmpScore = historyScore(i); historyScore(i) = historyScore(j); historyScore(j) = tmpScore
    tmpByte = flags(i); flags(i) = flags(j); flags(j) = tmpByte
