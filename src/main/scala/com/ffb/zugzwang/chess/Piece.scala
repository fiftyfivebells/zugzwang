package com.ffb.zugzwang.chess

enum PieceType(val name: String):
  case Pawn extends PieceType("p")
  case Knight extends PieceType("n")
  case Bishop extends PieceType("b")
  case Rook extends PieceType("r")
  case Queen extends PieceType("q")
  case King extends PieceType("k")

end PieceType

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

end Piece
