package com.ffb.zugzwang

import com.ffb.zugzwang.chess.GameState
import com.ffb.zugzwang.move.Move
import com.ffb.zugzwang.notation.{FENParser, FENParserError}
import com.ffb.zugzwang.rules.Rules

object Zugzwang:
  def initial: GameState = GameState.initial

  def gamestateFrom(fen: String): Either[FENParserError, GameState] =
    FENParser.from(fen)

  def applyMove(fen: String, move: Move): Either[FENParserError, GameState] =
    FENParser.from(fen).map(Rules.applyMove(_, move))

  def legalMoves(fen: String): Either[FENParserError, Seq[Move]] =
    FENParser.from(fen).map(Rules.legalMoves(_))

  def legalMoves(state: GameState): Seq[Move] =
    Rules.legalMoves(state)

class Zugzwang:
  private var state: GameState = GameState.initial

  def load(fen: String = GameState.initialFEN): Unit =
    state = GameState.from(fen)

  def reset: Unit = state = GameState.initial

  def applyMove(move: Move): Unit =
    state = Rules.applyMove(state, move)

  def legalMoves: Seq[Move] = Rules.legalMoves(state)
