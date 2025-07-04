package com.ffb.zugzwang.chess

import com.ffb.zugzwang.board.PieceCategory

enum PieceType(val name: String):
  case Pawn extends PieceType("p")
  case Knight extends PieceType("n")
  case Bishop extends PieceType("b")
  case Rook extends PieceType("r")
  case Queen extends PieceType("q")
  case King extends PieceType("k")

end PieceType

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

case class Piece(color: Color, pieceType: PieceType):
  override def toString: String = color match
    case Color.White => pieceType.name.toUpperCase
    case Color.Black => pieceType.name

end Piece

object Piece:

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

    Piece(color, pieceType)

  def from(pc: PieceCategory): Piece = pc match {
    case PieceCategory.WP => Piece(Color.White, PieceType.Pawn)
    case PieceCategory.WN => Piece(Color.White, PieceType.Knight)
    case PieceCategory.WB => Piece(Color.White, PieceType.Bishop)
    case PieceCategory.WR => Piece(Color.White, PieceType.Rook)
    case PieceCategory.WQ => Piece(Color.White, PieceType.Queen)
    case PieceCategory.WK => Piece(Color.White, PieceType.King)
    case PieceCategory.BP => Piece(Color.Black, PieceType.Pawn)
    case PieceCategory.BN => Piece(Color.Black, PieceType.Knight)
    case PieceCategory.BB => Piece(Color.Black, PieceType.Bishop)
    case PieceCategory.BR => Piece(Color.Black, PieceType.Rook)
    case PieceCategory.BQ => Piece(Color.Black, PieceType.Queen)
    case PieceCategory.BK => Piece(Color.Black, PieceType.King)
  }

end Piece
