package com.ffb.zugzwang.move

import com.ffb.zugzwang.board.Bitboard
import com.ffb.zugzwang.chess.Square

object HQSlidingAttacks extends SlidingAttackGen:
  private def slidingMoves(
    sq: Square,
    occ: Bitboard,
    mask: Bitboard
  ): Bitboard =
    val startSquare = 1L << sq.value

    val bottom = ((occ & mask) - (startSquare << 1)) & mask
    val top =
      ((occ & mask).reverse - (Bitboard(startSquare).reverse) * 2L).reverse

    (bottom ^ top) & mask

  // DiagonalMasks takes in a square and returns the bitboard that masks the diagonal lines
  // (positive and negative) from that square. The formula for calculating this comes from this link:
  // https://www.chessprogramming.org/On_an_empty_Board#By_Calculation_3
  private val diagonalMasks = Array.tabulate(64) { sq =>
    val diagonal = 56L - 8L * (sq & 7L) - (sq & 56L)
    val north    = -diagonal & (diagonal >>> 31L)
    val south    = diagonal & (-diagonal >>> 31L)

    ((Bitboard.diagonal >>> south) << north) ^ (1L << sq)
  }

  // AntiDiagonalMask takes in a square and returns the bitboard that masks the antidiagonal lines
  // (positive and megative) from that square. The formula for calculating this comes from the link above.
  private val antiDiagonalMasks = Array.tabulate(64) { sq =>
    val antiDiagonal = 8L * (sq & 7L) - (sq & 56L)
    val north        = -antiDiagonal & (antiDiagonal >>> 31L)
    val south        = antiDiagonal & (-antiDiagonal >>> 31L)

    ((Bitboard.antiDiagonal >>> south) << north) ^ (1L << sq)
  }

  private val HorizontalMasks = Array.tabulate(64) { sq =>
    (Bitboard(0xff) << (sq & 56L)) ^ (1L << sq)
  }
  private val VerticalMasks = Array.tabulate(64) { sq =>
    Bitboard((0x0101010101010101L << (sq & 7L)) ^ (1L << sq))
  }

  def bishopAttacks(
    square: Square,
    occupied: Bitboard
  ): Bitboard =
    val diagonalMask     = diagonalMasks(square.value)
    val antiDiagonalMask = antiDiagonalMasks(square.value)

    val diagonal     = slidingMoves(square, occupied, diagonalMask)
    val antiDiagonal = slidingMoves(square, occupied, antiDiagonalMask)

    diagonal | antiDiagonal

  def rookAttacks(square: Square, occupied: Bitboard): Bitboard =
    val rankMask = HorizontalMasks(square.value)
    val fileMask = VerticalMasks(square.value)

    val rank = slidingMoves(square, occupied, rankMask)
    val file = slidingMoves(square, occupied, fileMask)

    rank | file
