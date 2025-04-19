package com.ffb.zugzwang.chess

opaque type File = Int

object File:
  extension (file: File)
    inline def value: Int = file

    inline def >(inline other: File): Boolean = value > other.value
    inline def >=(inline other: File): Boolean = value >= other.value
    inline def <(inline other: File): Boolean = value < other.value
    inline def <=(inline other: File): Boolean = value <= other.value

    inline def toChar: Char = ('a' + value).toChar

  end extension

  inline def apply(x: Int): Either[String, File] = if x < 0 || x > 7 then
    Left(s"Integer $x ouf of valid range for file.")
  else Right(x)

  inline def of(sq: Square): File = sq.value % 8

end File
