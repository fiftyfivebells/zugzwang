package com.ffb.zugzwang.move

import com.ffb.zugzwang.chess.{Color, Piece, PieceType, Square}
import com.ffb.zugzwang.board.Bitboard

trait SlidingAttackGen:
  def bishopAttacks(square: Square, color: Color, occupied: Bitboard): Bitboard
  def rookAttacks(square: Square, color: Color, occupied: Bitboard): Bitboard

object PawnAttacks:
  val white: IArray[Bitboard] = IArray.tabulate(64) { sq =>
    val board = 1L << sq
    val rightAttack = ~Bitboard.fileA & (board << 7)
    val leftAttack = ~Bitboard.fileH & (board << 9)

    leftAttack | rightAttack
  }

  val black: IArray[Bitboard] = IArray.tabulate(64) { sq =>
    val board = 1L << sq
    val rightAttack = ~Bitboard.fileA & (board >>> 9)
    val leftAttack = ~Bitboard.fileH & (board >>> 7)

    leftAttack | rightAttack
  }

end PawnAttacks

object KnightAttacks:
  private val NotFileAOrB = ~(Bitboard.fileA | Bitboard.fileB)
  private val NotFileHOrG = ~(Bitboard.fileH | Bitboard.fileG)

  val table: IArray[Bitboard] = IArray.tabulate(64) { sq =>
    val startSquare = 1L << sq

    val northNorthWest = ~Bitboard.fileH & (startSquare << 17)
    val northNorthEast = ~Bitboard.fileA & (startSquare << 15)

    val eastEastNorth = NotFileAOrB & (startSquare << 6)
    val eastEastSouth = NotFileAOrB & (startSquare >> 10)

    val westWestNorth = NotFileHOrG & (startSquare << 10)
    val westWestSouth = NotFileHOrG & (startSquare >> 6)

    val southSouthEast = ~Bitboard.fileA & (startSquare >> 17)
    val southSouthWest = ~Bitboard.fileH & (startSquare >> 15)

    northNorthWest | northNorthEast | eastEastNorth | eastEastSouth | westWestNorth | westWestSouth | southSouthEast | southSouthWest
  }

end KnightAttacks

object KingAttacks:
  val table: IArray[Bitboard] = IArray.tabulate(64) { sq =>
    val startSquare = 1L << sq

    val north = Bitboard(startSquare << 8)
    val northEast = ~Bitboard.fileA & (startSquare << 7)
    val east = ~Bitboard.fileA & (startSquare >>> 1)
    val southEast = ~Bitboard.fileA & (startSquare >>> 9)
    val south = Bitboard(startSquare >>> 8)
    val southWest = ~Bitboard.fileH & (startSquare >>> 7)
    val west = ~Bitboard.fileH & (startSquare << 1)
    val northWest = ~Bitboard.fileH & (startSquare << 9)

    north | northEast | east | southEast | south | southWest | west | northWest
  }
end KingAttacks

object Attacks:

  private val sliders = HQSlidingAttacks

  inline def attacks(piece: Piece, from: Square, occupied: Bitboard): Bitboard =
    piece match {
      case Piece(Color.White, PieceType.Pawn) => PawnAttacks.white(from.value)
      case Piece(Color.Black, PieceType.Pawn) => PawnAttacks.black(from.value)
      case Piece(_, PieceType.Knight)         => KnightAttacks.table(from.value)
      case p @ Piece(_, PieceType.Bishop) =>
        sliders.bishopAttacks(from, p.color, occupied)
      case p @ Piece(_, PieceType.Rook) =>
        sliders.rookAttacks(from, p.color, occupied)
      case p @ Piece(_, PieceType.Queen) =>
        sliders.bishopAttacks(from, p.color, occupied) |
          sliders.rookAttacks(from, p.color, occupied)
      case Piece(_, PieceType.King) => KingAttacks.table(from.value)
    }

end Attacks
