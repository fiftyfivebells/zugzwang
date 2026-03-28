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

  private inline val RookOpenFileMg     = 25
  private inline val RookOpenFileEg     = 15
  private inline val RookSemiOpenFileMg = 12
  private inline val RookSemiOpenFileEg = 8

  private inline val ShieldMissingMg  = 20
  private inline val ShieldAdvancedMg = 10

  // passed pawn bonus — marginal premium for being unblockable, on top of PST advancement signal
  // index = rank from pawn's perspective (0=rank1, 7=rank8, both impossible)
  private val PassedPawnMgBonus = Array(0, 0, 3, 8, 15, 30, 55, 0)
  private val PassedPawnEgBonus = Array(0, 0, 5, 12, 25, 45, 80, 0)

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

    // rook on open/semi-open file
    val wRooks = position.pieces(Piece.WhiteRook)
    val bRooks = position.pieces(Piece.BlackRook)

    wRooks.foreach { sq =>
      val fileMask = Bitboard.fileH << (sq.toInt & 7).toLong
      if (fileMask & wPawns).isEmpty then
        if (fileMask & bPawns).isEmpty then
          mg += RookOpenFileMg; eg += RookOpenFileEg
        else
          mg += RookSemiOpenFileMg; eg += RookSemiOpenFileEg
    }

    bRooks.foreach { sq =>
      val fileMask = Bitboard.fileH << (sq.toInt & 7).toLong
      if (fileMask & bPawns).isEmpty then
        if (fileMask & wPawns).isEmpty then
          mg -= RookOpenFileMg; eg -= RookOpenFileEg
        else
          mg -= RookSemiOpenFileMg; eg -= RookSemiOpenFileEg
    }

    // king safety — pawn shield (midgame only; phase taper handles endgame fade)
    var wKingSq = 0
    position.pieces(Piece.WhiteKing).foreach(sq => wKingSq = sq.toInt)
    val wKingCol     = wKingSq & 7
    val wShieldRanks = Bitboard.rank2 | Bitboard.rank3
    var wCol         = (wKingCol - 1).max(0)
    while wCol <= (wKingCol + 1).min(7) do
      val fileMask    = Bitboard.fileH << wCol.toLong
      val pawnsOnFile = fileMask & wPawns
      if pawnsOnFile.isEmpty then mg -= ShieldMissingMg
      else if (pawnsOnFile & wShieldRanks).isEmpty then mg -= ShieldAdvancedMg
      wCol += 1

    var bKingSq = 0
    position.pieces(Piece.BlackKing).foreach(sq => bKingSq = sq.toInt)
    val bKingCol     = bKingSq & 7
    val bShieldRanks = Bitboard.rank6 | Bitboard.rank7
    var bCol         = (bKingCol - 1).max(0)
    while bCol <= (bKingCol + 1).min(7) do
      val fileMask    = Bitboard.fileH << bCol.toLong
      val pawnsOnFile = fileMask & bPawns
      if pawnsOnFile.isEmpty then mg += ShieldMissingMg
      else if (pawnsOnFile & bShieldRanks).isEmpty then mg += ShieldAdvancedMg
      bCol += 1

    // Taper between midgame and endgame
    val phase = calculatePhase(position)
    val score = (mg * phase + eg * (TotalPhase - phase)) / TotalPhase
    if position.activeSide == Color.White then Score(score) else Score(-score)
