package com.ffb.zugzwang.evaluation

import com.ffb.zugzwang.chess.{Color, MutablePosition}

object Evaluation:
  val Checkmate: Int = 30000
  val Infinity: Int  = 32000

  def evaluate(position: MutablePosition): Int =
    var score = 0

    val pieces = position.squares
    var i      = 0
    while i < pieces.length do
      val piece = pieces(i)

      if !piece.isNoPiece then
        val value = piece.pieceType.value
        if piece.color == Color.White then score += value else score -= value

      i += 1

    if position.activeSide == Color.White then score else -score
