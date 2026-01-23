package com.ffb.zugzwang.chess

final class PositionState:
  var captured: Piece        = Piece.NoPiece
  var capturedSquare: Square = Square.H1
  var movedPiece: Piece      = Piece.NoPiece

  var prevCastleRights: CastleRights = CastleRights.initial
  var prevEnPassant: Option[Square]  = None
  var prevHalfMove: Int              = 0
  var prevFullMove: Int              = 1
