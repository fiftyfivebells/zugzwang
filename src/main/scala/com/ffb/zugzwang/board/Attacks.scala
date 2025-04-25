package com.ffb.zugzwang.board

import com.ffb.zugzwang.chess.Square
import com.ffb.zugzwang.board.Bitboard.antiDiagonal

object Attacks:
  private val North = -8
  private val NorthEast = -9
  private val East = -1
  private val SouthEast = 7
  private val South = 8
  private val SouthWest = 9
  private val West = 1
  private val NorthWest = -7

  private val NotFileAOrB = ~(Bitboard.fileA | Bitboard.fileB)
  private val NotFileHOrG = ~(Bitboard.fileH | Bitboard.fileG)

  private val KnightDirections = IArray[Int](
    North + North + West,
    North + North + East,
    East + East + North,
    East + East + South,
    South + South + East,
    South + South + West,
    West + West + South,
    West + West + North
  )
  private val KingDirections = IArray[Int](
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest
  )

  private val KnightMoves = IArray.fill(64)(BitboardBoard.empty)
  private val KingMoves = IArray.fill(64)(BitboardBoard.empty)
  private val BishopMoves = IArray.fill(64)(BitboardBoard.empty)
  private val RookMoves = IArray.fill(64)(BitboardBoard.empty)
  private val QueenMoves = IArray.fill(64)(BitboardBoard.empty)

  private val DiagonalMasks = IArray.fill(64)(BitboardBoard.empty)
  private val AntiDiagonalMasks = IArray.fill(64)(BitboardBoard.empty)
  private val HorizontalMasks = IArray.fill(64)(BitboardBoard.empty)
  private val VerticalMasks = IArray.fill(64)(BitboardBoard.empty)

  private def createDiagonalMask(sq: Square): Bitboard =
    val diagonal = 56L - 8L * (sq.value & 7L) - (sq.value & 56L)
    val north = -diagonal & (diagonal >>> 31L)
    val south = diagonal & (-diagonal >>> 31L)

    ((Bitboard.diagonal >>> south) << north) ^ (1L << sq.value)


  private def createAntiDiagonalMask(sq: Square): Bitboard =
    val antiDiagonal = 8L * (sq.value & 7L) - (sq.value & 56L)
    val north = -antiDiagonal & (antiDiagonal >>> 31L)
    val south = antiDiagonal & (-antiDiagonal >>> 31L)

    ((Bitboard.antiDiagonal >>> south) << north) ^ (1L << sq.value)


  private def createHorizontalMask(sq: Square): Bitboard =
    Bitboard(0xff << (sq.value & 56L)) ^ (1L << sq.value)


  private def createVerticalMask(sq: Square): Bitboard =
    Bitboard((0x0101010101010101L << (sq.value & 7L)) ^ (1L << sq.value))


  private def initializeMoveTables: Unit = ???

end Attacks
