package com.ffb.zugzwang.move

import com.ffb.zugzwang.chess.GameState
import com.ffb.zugzwang.board.Attacks
import com.ffb.zugzwang.rules.Rules

object MoveGenerator:
  private val moves: MoveList = MoveList(256)

  def legalMoves(state: GameState): List[Move] =
    pseudoLegalMoves(state) filter { move =>
      val newState = Rules.applyMove(state, move)
      Rules.isSideInCheck(state, state.activeSide)
    }

  def pseudoLegalMoves(state: GameState): List[Move] = ???
