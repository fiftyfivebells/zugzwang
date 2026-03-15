package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.{PieceType, Square}
import com.ffb.zugzwang.core.Score

final class ContinuationHistoryTable():
  private inline val NumPieceTypes = 6
  private inline val NumSquares    = 64
  private inline val AxisSize      = NumPieceTypes * NumSquares
  private inline val InnerStride   = NumSquares
  private inline val MaxScore      = 16384

  private val table = new Array[Score](AxisSize * AxisSize)

  def rawArray: Array[Score] = table

  inline def baseOffset(prevPieceType: PieceType, prevTo: Square): Int =
    (prevPieceType * NumSquares + prevTo.toInt) * AxisSize

  inline def get(base: Int, currPieceType: PieceType, currTo: Square): Score =
    table(base + currPieceType * InnerStride + currTo.toInt)

  def update(
    prevPieceType: PieceType,
    prevTo: Square,
    currPieceType: PieceType,
    currTo: Square,
    bonus: Score
  ): Unit =
    val idx = (prevPieceType * NumSquares + prevTo.toInt) * AxisSize + currPieceType * InnerStride + currTo.toInt

    val current = table(idx)
    val clamped = math.min(math.max(bonus.toInt, -MaxScore), MaxScore)
    table(idx) = current + clamped - current * math.abs(clamped) / MaxScore

  def clear(): Unit =
    for i <- 0 until table.size do table(i) = Score.Zero
