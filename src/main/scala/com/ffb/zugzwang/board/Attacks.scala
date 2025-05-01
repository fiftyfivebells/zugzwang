package com.ffb.zugzwang.board

import com.ffb.zugzwang.board.Bitboard.antiDiagonal
import com.ffb.zugzwang.chess.{Color, Piece, PieceType, Square}

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

object BishopAttacks:

  // DiagonalMasks takes in a square and returns the bitboard that masks the diagonal lines
  // (positive and negative) from that square. The formula for calculating this comes from this link:
  // https://www.chessprogramming.org/On_an_empty_Board#By_Calculation_3
  private val DiagonalMasks = Array.tabulate(64) { sq =>
    val diagonal = 56L - 8L * (sq & 7L) - (sq & 56L)
    val north = -diagonal & (diagonal >>> 31L)
    val south = diagonal & (-diagonal >>> 31L)

    ((Bitboard.diagonal >>> south) << north) ^ (1L << sq)
  }

  // AntiDiagonalMask takes in a square and returns the bitboard that masks the antidiagonal lines
  // (positive and megative) from that square. The formula for calculating this comes from the link above.
  private val AntiDiagonalMasks = Array.tabulate(64) { sq =>
    val antiDiagonal = 8L * (sq & 7L) - (sq & 56L)
    val north = -antiDiagonal & (antiDiagonal >>> 31L)
    val south = antiDiagonal & (-antiDiagonal >>> 31L)

    ((Bitboard.antiDiagonal >>> south) << north) ^ (1L << sq)
  }

  def apply(sq: Square, c: Color, occ: Bitboard): Bitboard =
    val diagonalMask = DiagonalMasks(sq.value)
    val antiDiagonalMask = AntiDiagonalMasks(sq.value)

    val diagonal = Attacks.slidingMoves(sq, c, occ, diagonalMask)
    val antiDiagonal = Attacks.slidingMoves(sq, c, occ, antiDiagonalMask)

    diagonal | antiDiagonal

end BishopAttacks

object RookAttacks:
  private val HorizontalMasks = Array.tabulate(64) { sq =>
    Bitboard(0xff << (sq & 56L)) ^ (1L << sq)
  }
  private val VerticalMasks = Array.tabulate(64) { sq =>
    Bitboard((0x0101010101010101L << (sq & 7L)) ^ (1L << sq))
  }

  def apply(sq: Square, c: Color, occ: Bitboard): Bitboard =
    val rankMask = HorizontalMasks(sq.value)
    val fileMask = VerticalMasks(sq.value)

    val rank = Attacks.slidingMoves(sq, c, occ, rankMask)
    val file = Attacks.slidingMoves(sq, c, occ, fileMask)

    rank | file

end RookAttacks

object QueenAttacks:

  def apply(sq: Square, c: Color, occ: Bitboard): Bitboard =
    BishopAttacks(sq, c, occ) | RookAttacks(sq, c, occ)

end QueenAttacks

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

  private[board] def slidingMoves(
      sq: Square,
      c: Color,
      occ: Bitboard,
      mask: Bitboard
  ): Bitboard =
    val startSquare = 1L << sq.value

    val bottom = ((occ & mask) - (startSquare << 1)) & mask
    val top =
      ((occ & mask).reverse - (Bitboard(startSquare).reverse) * 2L).reverse

    (bottom ^ top) & mask

  inline def attacks(piece: Piece, from: Square, occupied: Bitboard): Bitboard =
    piece match {
      case Piece(Color.White, PieceType.Pawn) => PawnAttacks.white(from.value)
      case Piece(Color.Black, PieceType.Pawn) => PawnAttacks.black(from.value)
      case Piece(_, PieceType.Knight)         => KnightAttacks.table(from.value)
      case p@Piece(_, PieceType.Bishop)         => BishopAttacks(from, p.color, occupied)
      case p@Piece(_, PieceType.Rook)           => RookAttacks(from, p.color, occupied)
      case p@Piece(_, PieceType.Queen)          => QueenAttacks(from, p.color, occupied)
      case Piece(_, PieceType.King)           => KingAttacks.table(from.value)
    }

end Attacks
