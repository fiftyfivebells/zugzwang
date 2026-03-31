package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.{MutablePosition, Piece}
import com.ffb.zugzwang.core.{Ply, Score}
import com.ffb.zugzwang.evaluation.SEE
import com.ffb.zugzwang.move.Move

// Per-move metadata pre-computed during move scoring, carried forward to search time.
// Avoids repeated pieceAt() calls and SEE invocations in the move loop.
final class MoveMetadata(capacity: Int):
  val moverPiece: Array[Piece]    = new Array[Piece](capacity)
  val capturedPiece: Array[Piece] = new Array[Piece](capacity)
  val historyScore: Array[Score]  = new Array[Score](capacity)
  val flags: Array[Byte]          = new Array[Byte](capacity)

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
    moverPiece(i) = piece

    var f: Int = 0
    if move.isCapture then
      f |= FlagCapture
      val victim = position.pieceAt(move.to)
      capturedPiece(i) = victim
      if SEE.seeGE(position, move) then f |= FlagGoodNoisy
    else capturedPiece(i) = Piece.NoPiece

    if move.isPromotion then f |= FlagPromotion
    if !move.isCapture && !move.isPromotion then f |= FlagQuiet

    flags(i) = f.toByte

    historyScore(i) =
      if !move.isCapture && !move.isPromotion then searchHistory.quietMoveScore(ply, move, piece)
      else Score.Zero

  def swap(i: Int, j: Int): Unit =
    var tmpPiece: Piece = Piece.NoPiece
    var tmpScore: Score = Score.Zero
    var tmpByte: Byte   = 0

    tmpPiece = moverPiece(i); moverPiece(i) = moverPiece(j); moverPiece(j) = tmpPiece
    tmpPiece = capturedPiece(i); capturedPiece(i) = capturedPiece(j); capturedPiece(j) = tmpPiece
    tmpScore = historyScore(i); historyScore(i) = historyScore(j); historyScore(j) = tmpScore
    tmpByte = flags(i); flags(i) = flags(j); flags(j) = tmpByte
