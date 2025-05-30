package com.ffb.zugzwang

import com.ffb.zugzwang.chess.GameState
import com.ffb.zugzwang.rules.Rules
import com.ffb.zugzwang.notation.FENParser
import com.ffb.zugzwang.move.Perft
import com.ffb.zugzwang.move.Move

object Zugzwang:
  def legalMoves(fen: String): Seq[Move] =
    FENParser.from(fen) match {
      case Right(state) => Rules.legalMoves(state)
      case _            => Nil
    }

  def perft(fen: String, depth: Int = 3): Long =
    val state = GameState.from(fen)

    Perft.perft(state, depth)

end Zugzwang

class Zugzwang:
  private var state: GameState = GameState.initial

  def load(fen: String = GameState.initialFEN): Unit =
    state = GameState.from(fen)

  def reset: Unit = state = GameState.initial

  def applyMove(move: Move): Unit =
    state = Rules.applyMove(state, move)

  def legalMoves: Seq[Move] = Rules.legalMoves(state)

end Zugzwang
