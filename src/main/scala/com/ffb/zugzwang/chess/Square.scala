package com.ffb.zugzwang.chess

opaque type Square = Int

// TODO: maybe implement custom error types to return instead of String in Either,
//       or possibly just use Option for simplicity's sake?
object Square:

  extension (sq: Square)
    def value: Int = sq
    def file: File = File.of(sq)
    def rank: Rank = Rank.of(sq)

  end extension

  // this uses 7 - file.value because the least significant bit in the bitboard is h1, which is the
  // largest file value. Subtracting 7 "reverses" the file so that the index of the file corresponds
  // with the index in the board, ie. file h goes from index 7 to index 0. I'm only doing this because
  // it makes my bit math easier (at least to me).
  def from(file: File, rank: Rank): Square =
    (rank.value * 8) + (7 - file.value)

  def from(file: Int, rank: Int): Either[String, Square] = for {
    file <- File(file)
    rank <- Rank(rank)
  } yield Square.from(file, rank)

  def from(x: Int): Option[Square] = Option.when(0 <= x && x < 64)(x)

  def toAlgebraic(sq: Square): String =
    val fileChar: Char = File.of(sq).toChar
    val rankChar: Char = Rank.of(sq).toChar

    s"$fileChar$rankChar"

  def fromAlgebraic(sq: String): Either[String, Square] =
    if sq.length == 2 then
      for {
        file <- File(sq.charAt(0) - 'a')
        rank <- Rank(sq.charAt(1) - '1')
      } yield Square.from(file, rank)
    else Left(s"The algebraic notation string $sq is too long.")

end Square
