package com.ffb.zugzwang.evaluation

import com.ffb.zugzwang.chess.{Color, MutablePosition, Piece, PieceType, Square}
import com.ffb.zugzwang.core.Score

import scala.annotation.tailrec

object PestoEvaluation:
  val PawnPhase   = 0
  val KnightPhase = 1
  val BishopPhase = 1
  val RookPhase   = 2
  val QueenPhase  = 4
  val TotalPhase  = 24

  val MidgameValues = Array(82, 337, 365, 477, 1025, 0)
  val EndgameValues = Array(94, 281, 297, 512, 936, 0)

  private inline def calculatePhase(position: MutablePosition): Int =
    val phase =
      position.pieceTypeCount(PieceType.Knight) * KnightPhase +
        position.pieceTypeCount(PieceType.Bishop) * BishopPhase +
        position.pieceTypeCount(PieceType.Rook) * RookPhase +
        position.pieceTypeCount(PieceType.Queen) * QueenPhase

    phase.min(TotalPhase)

  private inline def getPstValue(piece: Piece, square: Square, tables: Array[Array[Int]]): Int =
    PieceSquareTables.value(tables, piece, square)

  private inline def evaluateGamePhase(
    position: MutablePosition,
    pst: Array[Array[Int]],
    materialValues: Array[Int]
  ): Score =
    @tailrec
    def loop(totalScore: Score, square: Square): Score =
      if square == Square.NoSquare then totalScore
      else
        val piece = position.pieceAt(square)
        if !piece.isNoPiece then
          val pstValue = getPstValue(piece, square, pst)
          val value    = materialValues(piece.pieceType) + pstValue
          val newScore = if piece.isWhite then totalScore + value else totalScore - value

          loop(newScore, square.next)
        else loop(totalScore, square.next)

    loop(Score(0), Square.H1)

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

    val mgScore = evaluateGamePhase(position, PieceSquareTables.MidgamePieceSquareTables, MidgameValues)
    val egScore = evaluateGamePhase(position, PieceSquareTables.EndgamePieceSquareTables, EndgameValues)
    val phase   = calculatePhase(position)

    val score = (mgScore * phase + egScore * (TotalPhase - phase)) / TotalPhase

    if position.activeSide == Color.White then score else -score
