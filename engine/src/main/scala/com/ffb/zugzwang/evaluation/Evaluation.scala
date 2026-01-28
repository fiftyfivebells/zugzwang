package com.ffb.zugzwang.evaluation

import com.ffb.zugzwang.chess.{Color, MutablePosition, Piece, Square}
import com.ffb.zugzwang.core.Score

import scala.annotation.tailrec

object Evaluation:

  def evaluate(position: MutablePosition): Score =

    @tailrec
    def loop(totalScore: Score, pieceIndex: Int, pieces: Array[Piece]): Score =
      if pieceIndex >= pieces.size then totalScore
      else
        val piece = pieces(pieceIndex)

        if !piece.isNoPiece then
          val pstValue = PieceSquareTables.value(piece.pieceType, Square(pieceIndex), piece.color)
          val value    = piece.pieceType.value + pstValue
          val newScore = if piece.color == Color.White then totalScore + value else totalScore - value

          loop(newScore, pieceIndex + 1, pieces)
        else loop(totalScore, pieceIndex + 1, pieces)

    val score = loop(Score(0), 0, position.squares)
    if position.activeSide == Color.White then score else -score
