package com.ffb.zugzwang.board

import com.ffb.zugzwang.chess.Square
import com.ffb.zugzwang.board.Bitboard.antiDiagonal

object Attacks:
  private val NotFileAOrB = ~(Bitboard.fileA | Bitboard.fileB)
  private val NotFileHOrG = ~(Bitboard.fileH | Bitboard.fileG)

  private val KnightMoves = Array.fill(64)(Bitboard.empty)
  private val KingMoves = Array.fill(64)(Bitboard.empty)
  private val BishopMoves = Array.fill(64)(Bitboard.empty)
  private val RookMoves = Array.fill(64)(Bitboard.empty)
  private val QueenMoves = Array.fill(64)(Bitboard.empty)

  private val DiagonalMasks = Array.fill(64)(Bitboard.empty)
  private val AntiDiagonalMasks = Array.fill(64)(Bitboard.empty)
  private val HorizontalMasks = Array.fill(64)(Bitboard.empty)
  private val VerticalMasks = Array.fill(64)(Bitboard.empty)

  // createDiagonalMask takes in a square and returns the bitboard that masks the diagonal lines
  // (positive and negative) from that square. The formula for calculating this comes from this link:
  // https://www.chessprogramming.org/On_an_empty_Board#By_Calculation_3
  private def createDiagonalMask(sq: Square): Bitboard =
    val diagonal = 56L - 8L * (sq.value & 7L) - (sq.value & 56L)
    val north = -diagonal & (diagonal >>> 31L)
    val south = diagonal & (-diagonal >>> 31L)

    ((Bitboard.diagonal >>> south) << north) ^ (1L << sq.value)

  // createAntiDiagonalMask takes in a square and returns the bitboard that masks the antidiagonal lines
  // (positive and megative) from that square. The formula for calculating this comes from the link above.
  private def createAntiDiagonalMask(sq: Square): Bitboard =
    val antiDiagonal = 8L * (sq.value & 7L) - (sq.value & 56L)
    val north = -antiDiagonal & (antiDiagonal >>> 31L)
    val south = antiDiagonal & (-antiDiagonal >>> 31L)

    ((Bitboard.antiDiagonal >>> south) << north) ^ (1L << sq.value)

  private def createHorizontalMask(sq: Square): Bitboard =
    Bitboard(0xff << (sq.value & 56L)) ^ (1L << sq.value)

  private def createVerticalMask(sq: Square): Bitboard =
    Bitboard((0x0101010101010101L << (sq.value & 7L)) ^ (1L << sq.value))

  private def createKnightMovesFor(sq: Square): Bitboard =
    val startSquare = (1L << sq.value)

    val northNorthWest = ~Bitboard.fileH & (startSquare << 17)
    val northNorthEast = ~Bitboard.fileA & (startSquare << 15)

    val eastEastNorth = NotFileAOrB & (startSquare << 6)
    val eastEastSouth = NotFileAOrB & (startSquare >> 10)

    val westWestNorth = NotFileHOrG & (startSquare << 10)
    val westWestSouth = NotFileHOrG & (startSquare >> 6)

    val southSouthEast = ~Bitboard.fileA & (startSquare >> 17)
    val southSouthWest = ~Bitboard.fileH & (startSquare >> 15)

    northNorthWest | northNorthEast | eastEastNorth | eastEastSouth | westWestNorth | westWestSouth | southSouthEast | southSouthWest

  private def createKingMovesFor(sq: Square): Bitboard =
    val startSquare = (1L << sq.value)

    val north = Bitboard(startSquare << 8)
    val northEast = ~Bitboard.fileA & (startSquare << 7)
    val east = ~Bitboard.fileA & (startSquare >>> 1)
    val southEast = ~Bitboard.fileA & (startSquare >>> 9)
    val south = Bitboard(startSquare >>> 8)
    val southWest = ~Bitboard.fileH & (startSquare >>> 7)
    val west = ~Bitboard.fileH & (startSquare << 1)
    val northWest = ~Bitboard.fileH & (startSquare << 9)

    north | northEast | east | southEast | south | southWest | west | northWest

  private def initializeMoveTables: Unit =
    (0 until 64) foreach { sq =>
      val square = Square(sq)
      KnightMoves(sq) = createKnightMovesFor(square)
      KingMoves(sq) = createKingMovesFor(square)

      HorizontalMasks(sq) = createHorizontalMask(square)
      VerticalMasks(sq) = createVerticalMask(square)
      DiagonalMasks(sq) = createDiagonalMask(square)
      AntiDiagonalMasks(sq) = createAntiDiagonalMask(square)
    }

  initializeMoveTables

end Attacks
