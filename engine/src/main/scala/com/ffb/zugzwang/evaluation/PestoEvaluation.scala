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

  // === HCE Constants (tune via Texel) ===
  private inline val BishopPairBonusMg = 30
  private inline val BishopPairBonusEg = 50

  private inline def calculatePhase(position: MutablePosition): Int =
    val phase =
      position.pieceTypeCount(PieceType.Knight) * KnightPhase +
        position.pieceTypeCount(PieceType.Bishop) * BishopPhase +
        position.pieceTypeCount(PieceType.Rook) * RookPhase +
        position.pieceTypeCount(PieceType.Queen) * QueenPhase

    phase.min(TotalPhase)

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

  def evaluateMaterialScore(position: MutablePosition): (Int, Int) =

    @tailrec()
    def loop(pieceType: PieceType, score: (Int, Int)): (Int, Int) =
      if pieceType == PieceType.King then score
      else
        val white = position.pieces(pieceType).popCount
        val black = position.pieces(pieceType + 6).popCount

        val midGame = score._1 + MidgameValues(pieceType) * (white - black)
        val endGame = score._2 + EndgameValues(pieceType) * (white - black)

        loop(PieceType(pieceType + 1), (midGame, endGame))

    loop(PieceType.Pawn, (0, 0))

  def evaluate(position: MutablePosition): Score =
    if isInsufficientMaterial(position) then return Score.Draw

    var (mg, eg) = evaluateMaterialScore(position)

    // PST scores (iterate occupied squares only — faster than 64-square loop)
    position.occupied.foreach { sq =>
      val piece = position.pieceAt(sq)
      val mgPst = PieceSquareTables.value(PieceSquareTables.MidgamePieceSquareTables, piece, sq)
      val egPst = PieceSquareTables.value(PieceSquareTables.EndgamePieceSquareTables, piece, sq)
      if piece.isWhite then
        mg += mgPst; eg += egPst
      else
        mg -= mgPst; eg -= egPst
    }

    // Material counts (reused for both material score and piece activity terms)
    val wBishops = position.pieces(Piece.WhiteBishop).popCount
    val bBishops = position.pieces(Piece.BlackBishop).popCount

    // Bishop pair bonus
    if wBishops >= 2 then
      mg += BishopPairBonusMg
      eg += BishopPairBonusEg
    if bBishops >= 2 then
      mg -= BishopPairBonusMg
      eg -= BishopPairBonusEg

    // Taper between midgame and endgame
    val phase = calculatePhase(position)
    val score = (mg * phase + eg * (TotalPhase - phase)) / TotalPhase
    if position.activeSide == Color.White then Score(score) else Score(-score)
