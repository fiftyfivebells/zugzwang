package com.ffb.zugzwang.chess

opaque type PieceType <: Int = Int
object PieceType:
  val Pawn: PieceType   = 0
  val Knight: PieceType = 1
  val Bishop: PieceType = 2
  val Rook: PieceType   = 3
  val Queen: PieceType  = 4
  val King: PieceType   = 5
  val NoType: PieceType = 6

  extension (pt: PieceType)
    def name: String = pt match
      case NoType => ""
      case Pawn   => "p"
      case Knight => "n"
      case Bishop => "b"
      case Rook   => "r"
      case Queen  => "q"
      case King   => "k"

    def value: Int = pt match
      case NoType => 0
      case Pawn   => 100
      case Knight => 320
      case Bishop => 330
      case Rook   => 500
      case Queen  => 900
      case King   => 20000

  def apply(piece: Int) = piece match
    case 0 => Pawn
    case 1 => Knight
    case 2 => Bishop
    case 3 => Rook
    case 4 => Queen
    case 5 => King
    case 6 => NoType

  def fromString(s: String): PieceType = s.toLowerCase match
    case "p" => Pawn
    case "n" => Knight
    case "b" => Bishop
    case "r" => Rook
    case "q" => Queen
    case "k" => King
    case _   => NoType

opaque type Piece <: Int = Int
object Piece:
  val WhitePawn: Piece   = 0
  val WhiteKnight: Piece = 1
  val WhiteBishop: Piece = 2
  val WhiteRook: Piece   = 3
  val WhiteQueen: Piece  = 4
  val WhiteKing: Piece   = 5
  val BlackPawn: Piece   = 6
  val BlackKnight: Piece = 7
  val BlackBishop: Piece = 8
  val BlackRook: Piece   = 9
  val BlackQueen: Piece  = 10
  val BlackKing: Piece   = 11
  val NoPiece: Piece     = 12

  private val whitePieces: Array[Piece] =
    Array(WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing)

  private val blackPieces: Array[Piece] =
    Array(BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing)

  def byColor(c: Color): Array[Piece] =
    if c == Color.White then whitePieces else blackPieces

  extension (p: Piece)

    inline def pieceType: PieceType = p % 6
    inline def isWhite: Boolean     = p >= 0 && p < 6
    inline def isBlack: Boolean     = p >= 6 && p < 12

    inline def color: Color =
      if isWhite then Color.White
      else if isBlack then Color.Black
      else throw new Exception(s"Called .color on NoPiece ($p)")

    inline def isPawn: Boolean    = (p % 6) == PieceType.Pawn
    inline def isKnight: Boolean  = (p % 6) == PieceType.Knight
    inline def isBishop: Boolean  = (p % 6) == PieceType.Bishop
    inline def isRook: Boolean    = (p % 6) == PieceType.Rook
    inline def isQueen: Boolean   = (p % 6) == PieceType.Queen
    inline def isKing: Boolean    = (p % 6) == PieceType.King
    inline def isNoPiece: Boolean = p == 12

    inline def toUciChar: Char =
      if p.isNoPiece then '.'
      else
        val base = p.pieceType match
          case PieceType.Pawn   => 'p'
          case PieceType.Knight => 'n'
          case PieceType.Bishop => 'b'
          case PieceType.Rook   => 'r'
          case PieceType.Queen  => 'q'
          case PieceType.King   => 'k'
          case _                => ' '

        if p.isWhite then base.toUpper else base

    inline def toStringRep: String =
      toUciChar.toString

  def from(c: Char): Piece =
    val color = if c.isUpper then Color.White else Color.Black
    val pieceType = c.toLower match
      case 'p' => PieceType.Pawn
      case 'n' => PieceType.Knight
      case 'b' => PieceType.Bishop
      case 'r' => PieceType.Rook
      case 'q' => PieceType.Queen
      case 'k' => PieceType.King

    (color.ordinal * 6) + pieceType

  def from(color: Color, pieceType: PieceType): Piece = (color.ordinal * 6) + pieceType
