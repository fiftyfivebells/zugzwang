package com.ffb.zugzwang.board

import com.ffb.zugzwang.chess.{Color, Piece, Square}

sealed trait Board extends Product with Serializable:
  def fromFen(fen: String): Board
  def toFen: String
    def clearBoard: Board
  def pieceAt(sq: Square): Piece
  def putPieceAt(p: Piece, sq: Square): Board
  def removePieceFrom(sq: Square): Board
  def isKingAttacked(c: Color): Boolean
  def isAttacked(sq: Square, c: Color): Boolean
  def isAttackedByPawn(sq: Square, c: Color): Boolean

end Board
