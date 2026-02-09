package com.ffb.zugzwang.chess

opaque type Square = Int

// TODO: maybe implement custom error types to return instead of String in Either,
//       or possibly just use Option for simplicity's sake?
object Square:

  extension (sq: Square)
    def value: Int = sq
    def file: File = File.of(sq)
    def rank: Rank = Rank.of(sq)

    def lastRank(c: Color): Boolean = c match
      case Color.White => rank.value == 7
      case Color.Black => rank.value == 0

    def startingPawnRank(c: Color): Boolean = c match
      case Color.White => rank.value == 1
      case Color.Black => rank.value == 6

    def next: Square =
      if sq == 64 then Square(0)
      else Square(sq + 1)

  def apply(x: Int): Square = x

  // this uses 7 - file.value because the least significant bit in the bitboard is h1, which is the
  // largest file value. Subtracting 7 "reverses" the file so that the index of the file corresponds
  // with the index in the board, ie. file h goes from index 7 to index 0. I'm only doing this because
  // it makes my bit math easier (at least to me).
  def from(file: File, rank: Rank): Square =
    (rank.value * 8) + (7 - file.value)

  def from(file: Int, rank: Int): Either[String, Square] = for
    file <- File(file)
    rank <- Rank(rank)
  yield Square.from(file, rank)

  def from(x: Int): Option[Square] = Option.when(0 <= x && x < 64)(x)

  def toAlgebraic(sq: Square): String =
    val fileChar: Char = File.of(sq).toChar
    val rankChar: Char = Rank.of(sq).toChar

    s"$fileChar$rankChar"

  def fromAlgebraic(sq: String): Either[String, Square] =
    if sq.length == 2 then
      for
        file <- File(sq.charAt(0) - 'a')
        rank <- Rank(sq.charAt(1) - '1')
      yield Square.from(file, rank)
    else Left(s"The algebraic notation string $sq is too long.")

  val H1: Square       = 0
  val G1: Square       = 1
  val F1: Square       = 2
  val E1: Square       = 3
  val D1: Square       = 4
  val C1: Square       = 5
  val B1: Square       = 6
  val A1: Square       = 7
  val H2: Square       = 8
  val G2: Square       = 9
  val F2: Square       = 10
  val E2: Square       = 11
  val D2: Square       = 12
  val C2: Square       = 13
  val B2: Square       = 14
  val A2: Square       = 15
  val H3: Square       = 16
  val G3: Square       = 17
  val F3: Square       = 18
  val E3: Square       = 19
  val D3: Square       = 20
  val C3: Square       = 21
  val B3: Square       = 22
  val A3: Square       = 23
  val H4: Square       = 24
  val G4: Square       = 25
  val F4: Square       = 26
  val E4: Square       = 27
  val D4: Square       = 28
  val C4: Square       = 29
  val B4: Square       = 30
  val A4: Square       = 31
  val H5: Square       = 32
  val G5: Square       = 33
  val F5: Square       = 34
  val E5: Square       = 35
  val D5: Square       = 36
  val C5: Square       = 37
  val B5: Square       = 38
  val A5: Square       = 39
  val H6: Square       = 40
  val G6: Square       = 41
  val F6: Square       = 42
  val E6: Square       = 43
  val D6: Square       = 44
  val C6: Square       = 45
  val B6: Square       = 46
  val A6: Square       = 47
  val H7: Square       = 48
  val G7: Square       = 49
  val F7: Square       = 50
  val E7: Square       = 51
  val D7: Square       = 52
  val C7: Square       = 53
  val B7: Square       = 54
  val A7: Square       = 55
  val H8: Square       = 56
  val G8: Square       = 57
  val F8: Square       = 58
  val E8: Square       = 59
  val D8: Square       = 60
  val C8: Square       = 61
  val B8: Square       = 62
  val A8: Square       = 63
  val NoSquare: Square = 64
