package com.ffb.zugzwang.move

import com.ffb.zugzwang.chess.GameState
import com.ffb.zugzwang.rules.Rules

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

object Perft:

  def perftSequential(state: GameState, depth: Int): Long =
    @tailrec
    def loop(stack: List[(GameState, Int)], acc: Long): Long = stack match
      case Nil => acc
      case (s, d) :: rest =>
        if d == 0 then loop(rest, acc + 1)
        else
          val nextStates = MoveGenerator.legalMoves(s) map { move =>
            (Rules.applyMove(s, move), d - 1)
          }
          loop(nextStates ++ rest, acc)

    loop(List((state, depth)), 0)

  def perft(state: GameState, depth: Int): Long =
    if depth == 0 then 1L
    else
      MoveGenerator
        .legalMoves(state)
        .toList
        .par
        .map { move =>
          val newState = Rules.applyMove(state, move)
          perftSequential(newState, depth - 1)
        }
        .sum

  def perftBasic(state: GameState, depth: Int): Long =
    if depth == 0 then 1
    else
      MoveGenerator
        .legalMoves(state)
        .map(move => perftBasic(Rules.applyMove(state, move), depth - 1))
        .sum

  def perftTrace(state: GameState, depth: Int, trace: List[Move] = Nil): Long =
    if depth == 0 then
      println("TRACE: " + trace.reverse.map(_.toUci).mkString(" "))
      1
    else
      MoveGenerator
        .legalMoves(state)
        .map(move => perftTrace(Rules.applyMove(state, move), depth - 1, move :: trace))
        .sum

  def divide(state: GameState, depth: Int): Long =
    val moves = MoveGenerator.legalMoves(state)
    var total = 0L

    for move <- moves do
      val nextState = Rules.applyMove(state, move)
      val nodes     = perftBasic(nextState, depth - 1)
      println(s"${move.toUci}: $nodes")
      total += nodes

    println(s"Total Nodes: $total")
    total
