package com.ffb.zugzwang.board

final case class Square private (file: Int, rank: Int)

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
    "TODO"

  def fromAlgebraic(sq: String): Square =
    Square(0, 0)

end Square
