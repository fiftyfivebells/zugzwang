package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.MutablePosition
import com.ffb.zugzwang.evaluation.Evaluation
import com.ffb.zugzwang.move.{Move, MoveGenerator}

import scala.annotation.tailrec

object Search:
  def findBestMoveIterative(position: MutablePosition, depth: Int): Move =
    var bestMove: Move = Move.None
    var alpha          = -Evaluation.Infinity
    val beta           = Evaluation.Infinity

    val moves = MoveGenerator.pseudoLegalMovesMutable(position)

    // TODO: sort moves here

    var i = 0
    while i < moves.size do
      val move = moves(i)

      position.applyMove(move)

      if !position.isKingAttacked(position.activeSide.enemy) then
        val score = -negamax(position, depth - 1, -beta, -alpha)

        if score > alpha then
          alpha = score
          bestMove = move

      position.unapplyMove(move)
      i += 1

    println(s"info score cp $alpha depth $depth")
    bestMove

  def findBestMove(position: MutablePosition, depth: Int): Move =
    val moves = MoveGenerator.pseudoLegalMovesMutable(position)

    // TODO: sort moves here

    @tailrec
    def loop(moves: List[Move], bestMove: Move, alpha: Int, beta: Int): Move =
      moves match
        case Nil =>
          println(s"info score cp $alpha depth $depth")
          bestMove
        case move :: rest =>
          position.applyMove(move)
          if position.isKingAttacked(position.activeSide.enemy) then
            position.unapplyMove(move)

            loop(rest, bestMove, alpha, beta)
          else
            val score                   = -negamax(position, depth - 1, -beta, -alpha)
            val (newAlpha, newBestMove) = if score > alpha then (score, move) else (alpha, bestMove)

            position.unapplyMove(move)

            loop(rest, newBestMove, newAlpha, beta)

    loop(moves, Move.None, -Evaluation.Infinity, Evaluation.Infinity)

  // TODO: look into maybe making this a tail recursive function
  private def negamax(position: MutablePosition, depth: Int, alpha: Int, beta: Int): Int =
    if depth == 0 then return Evaluation.evaluate(position)

    val moves = MoveGenerator.pseudoLegalMovesMutable(position)

    var bestScore       = -Evaluation.Infinity
    var currentAlpha    = alpha
    var legalMovesFound = 0

    var i = 0
    while i < moves.size do
      val move = moves(i)

      position.applyMove(move)

      if !position.isKingAttacked(position.activeSide.enemy) then
        legalMovesFound += 1

        val score = -negamax(position, depth - 1, -beta, -currentAlpha)

        position.unapplyMove(move)

        if score >= beta then return beta

        if score > bestScore then
          bestScore = score
          if score > currentAlpha then currentAlpha = score
      else position.unapplyMove(move) // illegal move

      i += 1

    if legalMovesFound == 0 then
      if position.isKingAttacked(position.activeSide) then return -Evaluation.Checkmate + (100 - depth)
      else return 0

    bestScore
