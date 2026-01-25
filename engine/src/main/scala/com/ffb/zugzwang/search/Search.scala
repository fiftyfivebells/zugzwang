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

    // TODO: should probably just return MoveList so I don't have to cast to IndexedSeq
    val sortedMoves = MoveSorter.sortMoves(moves.toIndexedSeq, position)

    var i = 0
    while i < sortedMoves.size do
      val move = sortedMoves(i)

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
    val moves       = MoveGenerator.pseudoLegalMovesMutable(position)
    val sortedMoves = MoveSorter.sortMoves(moves.toIndexedSeq, position)

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

    loop(sortedMoves.toList, Move.None, -Evaluation.Infinity, Evaluation.Infinity)

  // TODO: look into maybe making this a tail recursive function
  private def negamax(position: MutablePosition, depth: Int, alpha: Int, beta: Int): Int =
    if depth == 0 then return quiesce(position, alpha, beta)

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

  private def quiesce(position: MutablePosition, alpha: Int, beta: Int): Int =
    val standPat = Evaluation.evaluate(position)

    if standPat >= beta then beta
    else
      var currentAlpha = Math.max(alpha, standPat)

      val moves       = MoveGenerator.pseudoLegalCapturesMutable(position)
      val sortedMoves = MoveSorter.sortMoves(moves.toIndexedSeq, position)

      var i = 0
      while i < sortedMoves.size do
        val move = sortedMoves(i)

        position.applyMove(move)

        if !position.isKingAttacked(position.activeSide.enemy) then
          val score = -quiesce(position, -beta, -currentAlpha)

          position.unapplyMove(move)

          if score >= beta then return beta
          if score > currentAlpha then currentAlpha = score
        else position.unapplyMove(move)

        i += 1

      currentAlpha
