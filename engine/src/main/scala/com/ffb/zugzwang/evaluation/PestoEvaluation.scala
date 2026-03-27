package com.ffb.zugzwang.evaluation

import com.ffb.zugzwang.board.Bitboard
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

  // HCE constants (tune via Texel)
  private inline val BishopPairBonusMg = 30
  private inline val BishopPairBonusEg = 50

  // passed pawn marginal bonus — extra value of being unblockable, on top of PST
  // index = rank from pawn's own perspective (0=rank1, 7=rank8, both impossible for pawns)
  private val PassedPawnMgBonus = Array(0, 0, 0, 2, 5, 10, 20, 0)
  private val PassedPawnEgBonus = Array(0, 0, 2, 5, 10, 20, 40, 0)

  // precomputed passed pawn masks: own file + adjacent files, all ranks strictly ahead.
  // col = sq & 7: 0 = H-file, 7 = A-file. fileH << col gives the file mask for column col.
  private val WhitePassedPawnMasks: IArray[Bitboard] = IArray.tabulate(64) { sq =>
    val rank = sq >> 3
    val col  = sq & 7
    val files =
      Bitboard.fileH << col.toLong |
        (if col > 0 then Bitboard.fileH << (col - 1).toLong else Bitboard.empty) |
        (if col < 7 then Bitboard.fileH << (col + 1).toLong else Bitboard.empty)
    val ahead = if rank < 7 then Bitboard((-1L) << ((rank + 1) * 8)) else Bitboard.empty
    files & ahead
  }

  private val BlackPassedPawnMasks: IArray[Bitboard] = IArray.tabulate(64) { sq =>
    val rank = sq >> 3
    val col  = sq & 7
    val files =
      Bitboard.fileH << col.toLong |
        (if col > 0 then Bitboard.fileH << (col - 1).toLong else Bitboard.empty) |
        (if col < 7 then Bitboard.fileH << (col + 1).toLong else Bitboard.empty)
    val behind = if rank > 0 then Bitboard((1L << (rank * 8)) - 1) else Bitboard.empty
    files & behind
  }

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

    // bishop pair bonus
    if wBishops >= 2 then
      mg += BishopPairBonusMg
      eg += BishopPairBonusEg
    if bBishops >= 2 then
      mg -= BishopPairBonusMg
      eg -= BishopPairBonusEg

    // passed pawn bonus (marginal: extra value of being unblockable, on top of PST)
    val wPawns = position.pieces(Piece.WhitePawn)
    val bPawns = position.pieces(Piece.BlackPawn)

    wPawns.foreach { sq =>
      if (WhitePassedPawnMasks(sq.toInt) & bPawns).isEmpty then
        val rank = sq.toInt >> 3
        mg += PassedPawnMgBonus(rank)
        eg += PassedPawnEgBonus(rank)
    }

    bPawns.foreach { sq =>
      if (BlackPassedPawnMasks(sq.toInt) & wPawns).isEmpty then
        val rank = 7 - (sq.toInt >> 3)
        mg -= PassedPawnMgBonus(rank)
        eg -= PassedPawnEgBonus(rank)
    }

    // Taper between midgame and endgame
    val phase = calculatePhase(position)
    val score = (mg * phase + eg * (TotalPhase - phase)) / TotalPhase
    if position.activeSide == Color.White then Score(score) else Score(-score)
