package com.ffb.zugzwang.chess

opaque type Rank = Int

object Rank:
  extension (rank: Rank)
    inline def value: Int = rank

    inline def >(inline other: Rank): Boolean  = value > other.value
    inline def >=(inline other: Rank): Boolean = value >= other.value
    inline def <(inline other: Rank): Boolean  = value < other.value
    inline def <=(inline other: Rank): Boolean = value <= other.value

    inline def ==(inline other: Int): Boolean = value == other
    inline def !=(inline other: Int): Boolean = value != other

    inline def toChar: Char = ('1' + value).toChar
  end extension

  inline def apply(x: Int): Either[String, Rank] = if x < 0 || x > 7 then Left(s"Integer $x out of valid range for rank.")
  else Right(x)

  def of(sq: Square): Rank = sq.value / 8

end Rank
