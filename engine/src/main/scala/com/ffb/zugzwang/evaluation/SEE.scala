package com.ffb.zugzwang.evaluation

import com.ffb.zugzwang.board.Bitboard
import com.ffb.zugzwang.chess.{Color, MutablePosition, Piece, Square}
import com.ffb.zugzwang.move.{HQSlidingAttacks, KingAttacks, KnightAttacks, Move, MoveType, PawnAttacks}

import scala.annotation.tailrec

object SEE:
  private val gain = Array.ofDim[Int](32)

  def see(position: MutablePosition, move: Move): Int =
    val to   = move.to
    val from = move.from

    val (capturedSquare, captured) = if move.moveType == MoveType.EnPassant then
      val direction          = if position.activeSide == Color.White then -8 else 8
      val capturedPawnSquare = Square(to.value + direction)
      (capturedPawnSquare, position.pieceAt(capturedPawnSquare))
    else (to, position.pieceAt(to))

    if captured.isNoPiece then return 0

    val occupied =
      if move.moveType == MoveType.EnPassant then position.occupied.clearBitAt(capturedSquare).setBitAt(to)
      else position.occupied

    @tailrec
    def fill(
      depth: Int,
      activeSide: Color,
      attackerSquare: Square,
      victim: Piece,
      occupied: Bitboard
    ): Int =
      if attackerSquare == Square.NoSquare then return depth - 1

      val attacker = position.pieceAt(attackerSquare)

      if attacker.isNoPiece then depth - 1 // return the previous depth, because that was the last capture
      else
        gain(depth) = victim.materialValue

        val newOccupied        = occupied.clearBitAt(attackerSquare)
        val newAttackers       = getAllAttackers(position, to, newOccupied)
        val nextAttackerSquare = findLeastValuableAttacker(position, newAttackers, newOccupied, activeSide.enemy)

        fill(depth + 1, activeSide.enemy, nextAttackerSquare, attacker, newOccupied)

    @tailrec
    def resolve(depth: Int, currentScore: Int): Int =
      if depth < 0 then currentScore
      else
        val scoreAtDepth = gain(depth)
        val newScore =
          if depth == 0 then scoreAtDepth - currentScore
          else math.max(0, scoreAtDepth - currentScore)

        resolve(depth - 1, newScore)

    val maxDepth = fill(0, position.activeSide, from, captured, occupied)

    resolve(maxDepth, 0)

  def seeGE(position: MutablePosition, move: Move, threshold: Int = 0): Boolean =
    see(position, move) >= threshold

  private def getAllAttackers(
    position: MutablePosition,
    square: Square,
    occupied: Bitboard
  ): Bitboard =
    val diag  = HQSlidingAttacks.bishopAttacks(square, occupied)
    val ortho = HQSlidingAttacks.rookAttacks(square, occupied)
    val p     = position.pieces

    ((PawnAttacks.black(square.value) & p(Piece.WhitePawn)) |
      (PawnAttacks.white(square.value) & p(Piece.BlackPawn)) |
      (KnightAttacks.table(square.value) & (p(Piece.WhiteKnight) | p(Piece.BlackKnight))) |
      (diag & (p(Piece.WhiteBishop) | p(Piece.BlackBishop) | p(Piece.WhiteQueen) | p(Piece.BlackQueen))) |
      (ortho & (p(Piece.WhiteRook) | p(Piece.BlackRook) | p(Piece.WhiteQueen) | p(Piece.BlackQueen))) |
      (KingAttacks.table(square.value) & (p(Piece.WhiteKing) | p(Piece.BlackKing)))) & occupied

  private def findLeastValuableAttacker(
    position: MutablePosition,
    attackers: Bitboard,
    occupied: Bitboard,
    side: Color
  ): Square =
    val pieces = Piece.byColor(side)

    var i = 0
    while i < pieces.size do
      val piece      = pieces(i)
      val pieceBB    = position.pieces(piece)
      val candidates = attackers & pieceBB & occupied

      if candidates.nonEmpty then return candidates.leastSignificantBitUnsafe

      i += 1

    Square.NoSquare
