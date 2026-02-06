package com.ffb.zugzwang.evaluation

import com.ffb.zugzwang.chess.{Color, MutablePosition, Piece, Square}
import com.ffb.zugzwang.core.Score

import scala.annotation.tailrec

object Evaluation:

  private def isInsufficientMaterial(pos: MutablePosition): Boolean =
    // 1. if there are any pawns, rooks, or queens, it is NOT insufficient
    if (pos.pieces(Piece.WhitePawn) | pos.pieces(Piece.BlackPawn)).nonEmpty then return false
    if (pos.pieces(Piece.WhiteRook) | pos.pieces(Piece.BlackRook)).nonEmpty then return false
    if (pos.pieces(Piece.WhiteQueen) | pos.pieces(Piece.BlackQueen)).nonEmpty then return false

    // 2. count minor pieces (bishops and knights)
    val whiteMinors = (pos.pieces(Piece.WhiteBishop) | pos.pieces(Piece.WhiteKnight)).popCount
    val blackMinors = (pos.pieces(Piece.BlackBishop) | pos.pieces(Piece.BlackKnight)).popCount
    val totalMinors = whiteMinors + blackMinors

    // king vs. king (no minors)
    if totalMinors == 0 then return true

    // king + minor vs king
    if totalMinors == 1 then return true

    // TODO: k+b vs k+b is a draw, but the check is more complex. Implement this.

    false

  def evaluate(position: MutablePosition): Score =

    if isInsufficientMaterial(position) then return Score.Draw

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
