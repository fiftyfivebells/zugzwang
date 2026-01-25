package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.{MutablePosition, Piece}
import com.ffb.zugzwang.move.Move

object MoveSorter:
  def scoreMove(move: Move, position: MutablePosition): Int =
    val captured: Piece = position.pieceAt(move.to)

    // captures
    if !captured.isNoPiece then
      val mover       = position.pieceAt(move.from)
      val moverValue  = mover.pieceType.value
      val victimValue = captured.pieceType.value

      // MVV-LVA = most valuable victim/least valuable attacker
      // examples:
      // pawn takes queen => (900 * 10 - 100) = 8900 (greater priority)
      // queen takes pawn => (100 * 10 - 900) = 900
      10000 + (victimValue * 10) - moverValue

    // TODO: add another if/else block for killer moves

    // promotions have high values
    else if move.isPromotion then return 9000 + move.promotion.value

    // quiet moves are nothing special
    else 0

  def sortMoves(moves: IndexedSeq[Move], position: MutablePosition): IndexedSeq[Move] =
    // TODO: replace sortBy with something more performant
    moves.sortBy(m => -scoreMove(m, position))
