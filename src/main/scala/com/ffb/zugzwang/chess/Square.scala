package com.ffb.zugzwang.chess

final case class Square private (file: Int, rank: Int)

// TODO: maybe implement custom error types to return instead of String in Either
object Square:

  def from(file: Int, rank: Int): Either[String, Square] = (file, rank) match
    case (f, _) if f < 0 || f > 7 =>
      Left(
        s"Invalid file index, provide an integer between 0 and 7 (inclusive)."
      )
    case (_, r) if r < 0 || r > 7 =>
      Left(
        s"Invalid rank index, provide an integer between 0 and 7 (inclusive)."
      )
    case (f, r) => Right(Square(f, r))

  def toAlgebraic(sq: Square): String =
    val fileChar = ('a' + sq.file).toChar
    val rankChar = ('1' + sq.rank).toChar

    s"$fileChar$rankChar"

  def fromAlgebraic(sq: String): Either[String, Square] =
    if sq.length == 2 then
      val file = sq.charAt(0) - 'a'
      val rank = sq.charAt(1) - '1'
      Square.from(file, rank)
    else Left(s"The algebraic notation string is too long.")

end Square
