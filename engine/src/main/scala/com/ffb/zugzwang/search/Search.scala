package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.MutablePosition
import com.ffb.zugzwang.evaluation.Evaluation
import com.ffb.zugzwang.move.{Move, MoveGenerator}

import scala.annotation.tailrec

final case class SearchLimits(
  depth: Int = 100,
  moveTime: Long = Long.MaxValue,
  endTime: Long = Long.MaxValue
)

final case class SearchResult(
  move: Move,
  score: Int
)

final case class SearchContext(
  val startTime: Long,
  val endTime: Long,
  val depthLimit: Int,
  var nodes: Long = 0,
  var stopped: Boolean = false
)

object Search:
  def search(position: MutablePosition, limits: SearchLimits): Move =
    var bestMove: Move = Move.None
    val ctx = SearchContext(
      startTime = System.currentTimeMillis(),
      endTime = limits.endTime,
      depthLimit = limits.depth
    )

    var currentDepth = 1

    while currentDepth <= limits.depth && System.currentTimeMillis() < limits.endTime do
      val bestAtDepth = findBestMoveIterative(position, currentDepth, ctx)

      if System.currentTimeMillis() < limits.endTime then
        val result = bestAtDepth
        bestMove = result.move
        val scoreStr  = formatScore(result.score)
        val timeTaken = System.currentTimeMillis() - ctx.startTime
        val nps       = if timeTaken > 0 then (ctx.nodes * 1000) / timeTaken else 0

        println(s"info depth $currentDepth score $scoreStr nodes ${ctx.nodes} nps $nps time $timeTaken pv ${bestMove.toUci}")

      currentDepth += 1

    bestMove

  private def formatScore(score: Int): String =
    // With ply-based mate scores, mates are encoded as +/- (Checkmate - ply).
    val MateThreshold = Evaluation.Checkmate - 1000

    if score > MateThreshold then
      val pliesToMate = Evaluation.Checkmate - score
      val movesToMate = (pliesToMate + 1) / 2
      s"mate $movesToMate"
    else if score < -MateThreshold then
      val pliesToMate = Evaluation.Checkmate + score // score is negative here
      val movesToMate = (pliesToMate + 1) / 2
      s"mate -$movesToMate"
    else s"cp $score"

  def findBestMoveIterative(position: MutablePosition, depth: Int, ctx: SearchContext): SearchResult =
    var bestMove: Move = Move.None
    var alpha          = -Evaluation.Infinity
    val beta           = Evaluation.Infinity

    val moves = MoveGenerator.pseudoLegalMovesMutable(position)

    // TODO: should probably just return MoveList so I don't have to cast to IndexedSeq
    val sortedMoves = MoveSorter.sortMoves(moves, position)

    var i          = 0
    var legalMoves = 0
    while i < sortedMoves.size do
      val move = sortedMoves(i)

      position.applyMove(move)

      if !position.isSideInCheck(position.activeSide.enemy) then
        ctx.nodes += 1
        legalMoves += 1
        val score = -negamax(position, depth - 1, -beta, -alpha, ctx, ply = 1)

        if score > alpha then
          alpha = score
          bestMove = move

      position.unapplyMove(move)
      i += 1

    if legalMoves == 0 then
      if position.isSideInCheck(position.activeSide) then SearchResult(Move.None, -Evaluation.Checkmate)
      else SearchResult(Move.None, 0) // stalemate
    else SearchResult(bestMove, alpha)

  def findBestMove(position: MutablePosition, depth: Int, ctx: SearchContext): SearchResult =
    val moves       = MoveGenerator.pseudoLegalMovesMutable(position)
    val sortedMoves = MoveSorter.sortMoves(moves, position)

    @tailrec
    def loop(moves: List[Move], bestMove: Move, alpha: Int, beta: Int, legalMoves: Int, ply: Int): SearchResult =
      moves match
        case Nil =>
          if legalMoves == 0 then
            if position.isSideInCheck(position.activeSide) then SearchResult(Move.None, -Evaluation.Checkmate)
            else SearchResult(Move.None, 0)
          else SearchResult(bestMove, alpha)
        case move :: rest =>
          position.applyMove(move)
          if position.isSideInCheck(position.activeSide.enemy) then
            position.unapplyMove(move)

            loop(rest, bestMove, alpha, beta, legalMoves, ply + 1)
          else
            ctx.nodes += 1
            val score                   = -negamax(position, depth - 1, -beta, -alpha, ctx, ply + 1)
            val (newAlpha, newBestMove) = if score > alpha then (score, move) else (alpha, bestMove)
            position.unapplyMove(move)

            loop(rest, newBestMove, newAlpha, beta, legalMoves + 1, ply + 1)

    loop(sortedMoves.toList, Move.None, -Evaluation.Infinity, Evaluation.Infinity, 0, 1)

  // TODO: look into maybe making this a tail recursive function
  private def negamax(position: MutablePosition, depth: Int, alpha: Int, beta: Int, ctx: SearchContext, ply: Int): Int =
    if shouldStop(ctx) then return Evaluation.evaluate(position)

    if depth == 0 then return quiesce(position, alpha, beta, ctx, ply)

    val moves       = MoveGenerator.pseudoLegalMovesMutable(position)
    val sortedMoves = MoveSorter.sortMoves(moves, position)

    var bestScore       = -Evaluation.Infinity
    var currentAlpha    = alpha
    var legalMovesFound = 0

    var i = 0
    while i < sortedMoves.size do
      val move = sortedMoves(i)

      position.applyMove(move)

      if !position.isSideInCheck(position.activeSide.enemy) then
        ctx.nodes += 1
        legalMovesFound += 1

        val score = -negamax(position, depth - 1, -beta, -currentAlpha, ctx, ply + 1)

        position.unapplyMove(move)

        if score >= beta then return beta
        if score > bestScore then bestScore = score
        if score > currentAlpha then currentAlpha = score
      else position.unapplyMove(move) // illegal move

      i += 1

    if ctx.stopped then return currentAlpha

    if legalMovesFound == 0 then
      if position.isSideInCheck(position.activeSide) then -Evaluation.Checkmate + (100 - depth)
      else 0
    else bestScore

  private def quiesce(position: MutablePosition, alpha: Int, beta: Int, ctx: SearchContext, ply: Int): Int =
    if shouldStop(ctx) then Evaluation.evaluate(position)
    else if position.isSideInCheck(position.activeSide) then
      val moves           = MoveGenerator.pseudoLegalMovesMutable(position)
      var currentAlpha    = alpha
      var legalMovesFound = 0

      var i = 0
      while i < moves.size && !shouldStop(ctx) do
        val move = moves(i)
        position.applyMove(move)

        // after applyMove, side-to-move flips; check legality by ensuring the mover's king isn't in check
        if !position.isSideInCheck(position.activeSide.enemy) then
          ctx.nodes += 1
          legalMovesFound += 1

          val score = -quiesce(position, -beta, -currentAlpha, ctx, ply + 1)

          position.unapplyMove(move)

          if score >= beta then return beta
          if score > currentAlpha then currentAlpha = score
        else position.unapplyMove(move)

        i += 1

      if ctx.stopped then return currentAlpha

      // Checkmated (mate score is ply-based)
      if legalMovesFound == 0 then -Evaluation.Checkmate + ply
      else currentAlpha
    else
      val standPat = Evaluation.evaluate(position)

      if standPat >= beta then beta
      else
        var currentAlpha = Math.max(alpha, standPat)

        val moves       = MoveGenerator.pseudoLegalCapturesMutable(position)
        val sortedMoves = MoveSorter.sortMoves(moves, position)

        var i = 0
        while i < sortedMoves.size && !shouldStop(ctx) do
          val move = sortedMoves(i)

          position.applyMove(move)

          if !position.isSideInCheck(position.activeSide.enemy) then
            ctx.nodes += 1
            val score = -quiesce(position, -beta, -currentAlpha, ctx, ply + 1)

            position.unapplyMove(move)

            if score >= beta then return beta
            if score > currentAlpha then currentAlpha = score
          else position.unapplyMove(move)

          i += 1

        currentAlpha

  private inline def shouldStop(ctx: SearchContext): Boolean =
    if ctx.stopped then true
    else if (ctx.nodes & 2047L) == 0L && System.currentTimeMillis() >= ctx.endTime then
      ctx.stopped = true
      true
    else false
