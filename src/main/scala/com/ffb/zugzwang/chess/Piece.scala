package com.ffb.zugzwang.chess

enum PieceType(val name: String):
  case Pawn extends PieceType("p")
  case Knight extends PieceType("n")
  case Bishop extends PieceType("b")
  case Rook extends PieceType("r")
  case Queen extends PieceType("q")
  case King extends PieceType("k")

object PieceType:
  given Ordering[PieceType] with
    def compare(x: PieceType, y: PieceType): Int = value(y).compare(value(x))

  private def value(pt: PieceType): Int = pt match {
    case Pawn   => 10
    case Knight => 30
    case Bishop => 35
    case Rook   => 50
    case Queen  => 90
    case King   => 1000
  }

  def fromString(s: String): Option[PieceType] = s match {
    case "p" => Some(Pawn)
    case "n" => Some(Knight)
    case "b" => Some(Bishop)
    case "r" => Some(Rook)
    case "q" => Some(Queen)
    case "k" => Some(King)
    case _   => None
  }

opaque type Piece <: Int = Int
object Piece:
  val WhitePawn: Piece = 0
  val WhiteKnight: Piece = 1
  val WhiteBishop: Piece = 2
  val WhiteRook: Piece = 3
  val WhiteQueen: Piece = 4
  val WhiteKing: Piece = 5
  val BlackPawn: Piece = 6
  val BlackKnight: Piece = 7
  val BlackBishop: Piece = 8
  val BlackRook: Piece = 9
  val BlackQueen: Piece = 10
  val BlackKing: Piece = 11
  val NoPiece: Piece = -1

  private val pieceArray: Array[Piece] = Array(
    WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing,
    BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing
  )

  def byColor(c: Color): Array[Piece] = c match
    case Color.White =>
      Array(WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing)

    case Color.Black =>
      Array(BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing)

  extension (p: Piece)
    inline def color: Color =
      if p < 6 then Color.White else Color.Black

    inline def pieceType: PieceType = PieceType.fromOrdinal(p % 6)
    inline def isWhite: Boolean = p < 6
    inline def isBlack: Boolean = p > 5 && p < 12


  def from(c: Char): Piece =
    val color = if c.isUpper then Color.White else Color.Black
    val pieceType = c.toLower match {
      case 'p' => PieceType.Pawn
      case 'n' => PieceType.Knight
      case 'b' => PieceType.Bishop
      case 'r' => PieceType.Rook
      case 'q' => PieceType.Queen
      case 'k' => PieceType.King
    }

    pieceArray((color.ordinal * 6) + pieceType.ordinal)

  def from(color: Color, pieceType: PieceType) = pieceArray((color.ordinal * 6) + pieceType.ordinal)
