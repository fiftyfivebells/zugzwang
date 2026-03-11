package com.ffb.zugzwang.chess

opaque type Rank = Int

object Rank:
  extension (rank: Rank)
    inline def toInt: Int = rank

    inline def >(inline other: Rank): Boolean  = toInt > other.toInt
    inline def >=(inline other: Rank): Boolean = toInt >= other.toInt
    inline def <(inline other: Rank): Boolean  = toInt < other.toInt
    inline def <=(inline other: Rank): Boolean = toInt <= other.toInt

    inline def toChar: Char = ('1' + toInt).toChar

  inline def apply(x: Int): Either[String, Rank] = if x < 0 || x > 7 then Left(s"Integer $x out of valid range for rank.")
  else Right(x)

  def of(sq: Square): Rank = sq.toInt / 8
