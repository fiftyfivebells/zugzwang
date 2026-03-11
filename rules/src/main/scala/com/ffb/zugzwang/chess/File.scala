package com.ffb.zugzwang.chess

opaque type File = Int

object File:
  extension (file: File)
    inline def toInt: Int = file

    inline def >(inline other: File): Boolean  = toInt > other.toInt
    inline def >=(inline other: File): Boolean = toInt >= other.toInt
    inline def <(inline other: File): Boolean  = toInt < other.toInt
    inline def <=(inline other: File): Boolean = toInt <= other.toInt

    inline def toChar: Char = ('a' + toInt).toChar

  inline def apply(x: Int): Either[String, File] = if x < 0 || x > 7 then Left(s"Integer $x ouf of valid range for file.")
  else Right(x)

  inline def of(sq: Square): File = 7 - (sq.toInt % 8)
