package com.ffb.zugzwang.board

import com.ffb.zugzwang.chess.{Rank, Square}
import scala.annotation.targetName

opaque type Bitboard = Long

object Bitboard:
  val empty: Bitboard = 0L
  val full: Bitboard = 0xffffffffffffffffL

  val diagonal: Bitboard = 0x0102040810204080L
  val antiDiagonal: Bitboard = 0x8040201008040201L

  val fileA: Bitboard = 0x8080808080808080L
  val fileB: Bitboard = fileA >> 1
  val fileC: Bitboard = fileB >> 1
  val fileD: Bitboard = fileC >> 1
  val fileE: Bitboard = fileD >> 1
  val fileF: Bitboard = fileE >> 1
  val fileG: Bitboard = fileF >> 1
  val fileH: Bitboard = fileH >> 1

  val rank1: Bitboard = 0xffL
  val rank2: Bitboard = rank1 << 8
  val rank3: Bitboard = rank2 << 8
  val rank4: Bitboard = rank3 << 8
  val rank5: Bitboard = rank4 << 8
  val rank6: Bitboard = rank5 << 8
  val rank7: Bitboard = rank6 << 8
  val rank8: Bitboard = rank7 << 8

  val f1g1Mask: Bitboard = 6L
  val b1c1d1Mask: Bitboard = 0x70L
  val f8g8mask: Bitboard = 0x600000000000000L
  val b8c8d8Mask: Bitboard = 0x7000000000000000L

  def apply(n: Long): Bitboard = n

  def from(sq: Square): Bitboard = 1L << sq.value

  def from(sqs: Square*): Bitboard =
    sqs.foldLeft(0L: Bitboard)((bb, sq) => bb | (1 << sq.value))

  def fileMaskFor(sq: Square): Bitboard = sq.file.value match {
    case 0 => fileH
    case 1 => fileG
    case 2 => fileF
    case 3 => fileE
    case 4 => fileD
    case 5 => fileC
    case 6 => fileB
    case 7 => fileA
  }

  def ranKMaskFor(sq: Square): Bitboard = sq.rank.value match {
    case 0 => rank1
    case 1 => rank2
    case 2 => rank3
    case 3 => rank4
    case 4 => rank5
    case 5 => rank6
    case 6 => rank7
    case 7 => rank8
  }

  def reverse(bb: Bitboard): Bitboard = java.lang.Long.reverse(bb.value)

  extension (bb: Bitboard)

    inline def value: Long = bb
    inline def toLong: Long = bb

    inline def unary_~ : Bitboard = ~bb

    @targetName("and")
    inline def &(other: Long): Bitboard = bb & other
    inline def &(other: Bitboard): Bitboard = bb & other

    @targetName("or")
    inline def |(other: Long): Bitboard = bb | other
    inline def |(other: Bitboard): Bitboard = bb | other

    @targetName("xor")
    inline def ^(other: Long): Bitboard = bb ^ other
    inline def ^(other: Bitboard): Bitboard = bb ^ other

    @targetName("leftShift")
    inline def <<(other: Long): Bitboard = bb << other
    inline def <<(other: Bitboard): Bitboard = bb << other

    @targetName("rightShift")
    inline def >>>(other: Long): Bitboard = bb >>> other
    inline def >>>(other: Bitboard): Bitboard = bb >>> other

    inline def isEmpty: Boolean = bb == empty
    inline def nonEmpty: Boolean = bb != empty

    inline def setBitAt(sq: Square): Bitboard = bb | (1L << sq.value)

    inline def clearBitAt(sq: Square): Bitboard = bb & ~(1L << sq.value)

    def leastSignificantBit: Option[Square] =
      Square.from(java.lang.Long.numberOfTrailingZeros(bb))

    def removeLsb: Bitboard = bb & (bb - 1L)

    def popCount: Bitboard = java.lang.Long.bitCount(bb)

  end extension

end Bitboard
