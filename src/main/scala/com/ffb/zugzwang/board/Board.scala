package com.ffb.zugzwang.board

import com.ffb.zugzwang.chess.{Color, Piece, Square}

trait Board extends Product with Serializable:
  def toFen: String
  def clearBoard: Board
  def pieceAt(sq: Square): Option[Piece]
  def putPieceAt(p: Piece, sq: Square): Board
  def removePieceFrom(sq: Square): Board
  def isKingAttacked(c: Color): Boolean
  def isAttacked(sq: Square, c: Color): Boolean
  def isAttackedByPawn(sq: Square, c: Color): Boolean

end Board
