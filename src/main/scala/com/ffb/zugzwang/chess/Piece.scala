package com.ffb.zugzwang.chess

enum PieceType(val name: String):
  case Pawn extends PieceType("p")
  case Knight extends PieceType("n")
  case Bishop extends PieceType("b")
  case Rook extends PieceType("r")
  case Queen extends PieceType("q")
  case King extends PieceType("k")

end PieceType

case class Piece(pieceType: PieceType, color: Color):
  override def toString: String = color match
    case Color.White => pieceType.name
    case Color.Black => pieceType.name.toUpperCase

end Piece
