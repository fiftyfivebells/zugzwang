package com.ffb.zugzwang.chess

import com.ffb.zugzwang.chess.zobrist.ZobristHash

final class PositionUndoState:
  var captured: Piece        = Piece.NoPiece
  var capturedSquare: Square = Square.H1
  var movedPiece: Piece      = Piece.NoPiece

  var prevCastleRights: CastleRights = CastleRights.initial
  var prevEnPassant: Option[Square]  = None
  var prevHalfMove: Int              = 0
  var prevFullMove: Int              = 1

  var prevZobristHash: ZobristHash = ZobristHash.empty
