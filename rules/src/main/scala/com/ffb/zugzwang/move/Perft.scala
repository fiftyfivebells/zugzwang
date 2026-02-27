package com.ffb.zugzwang.move

import com.ffb.zugzwang.chess.GameState
import com.ffb.zugzwang.rules.Rules

import scala.collection.parallel.CollectionConverters.*

object Perft:

  /** Parallel perft — fast for benchmarking, parallelises at the root. */
  def perft(state: GameState, depth: Int): Long =
    if depth == 0 then 1L
    else
      MoveGenerator
        .legalMoves(state)
        .par
        .map(move => perftSequential(Rules.applyMove(state, move), depth - 1))
        .sum

  /** Sequential perft — used for sub-trees and as correctness reference. */
  def perftSequential(state: GameState, depth: Int): Long =
    if depth == 0 then 1L
    else
      MoveGenerator
        .legalMoves(state)
        .map(move => perftSequential(Rules.applyMove(state, move), depth - 1))
        .sum

  /** Divide — prints per-move node counts at the root. */
  def divide(state: GameState, depth: Int): Long =
    val moves = MoveGenerator.legalMoves(state)
    var total = 0L
    for move <- moves do
      val nodes = perftSequential(Rules.applyMove(state, move), depth - 1)
      println(s"${move.toUci}: $nodes")
      total += nodes
    println(s"\nTotal: $total")
    total
