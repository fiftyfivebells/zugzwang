package com.ffb.zugzwang.chess.zobrist

import com.ffb.zugzwang.chess.{CastleRights, Color, MutablePosition}

object Zobrist:
  def compute(position: MutablePosition): ZobristHash =
    var hash = ZobristHash.Empty

    val squares = position.squares

    var sq = 0
    while sq < 64 do
      val piece = squares(sq)
      if !piece.isNoPiece then hash = hash ^ ZobristKeys.pieceSquare(piece)(sq)
      sq += 1

    if position.activeSide == Color.Black then hash = hash ^ ZobristKeys.sideToMove

    hash = hash ^ ZobristKeys.castling(position.castleRights.maskValue)

    if position.enPassantSq.isDefined then hash = hash ^ ZobristKeys.epFile(position.enPassantSq.file.toInt)

    hash
