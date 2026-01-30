package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.MutablePosition
import com.ffb.zugzwang.core.{Depth, Node, Ply, Score, SearchTime, TimeControl}
import com.ffb.zugzwang.evaluation.Evaluation
import com.ffb.zugzwang.move.{Move, MoveGenerator}

import scala.annotation.tailrec

final case class SearchLimits(
  depth: Depth = Depth(100),
  moveTime: SearchTime = SearchTime.maxTime,
  endTime: SearchTime = SearchTime.maxTime
)

final case class SearchResult(
  move: Move,
  score: Score
)

final case class SearchContext(
  val startTime: SearchTime,
  val endTime: SearchTime,
  val depthLimit: Depth,
  var nodes: Node = Node.zero,
  var stopped: Boolean = false
)

object Search:
  def search(position: MutablePosition, limits: SearchLimits): Move =
    val now    = SearchTime.currentTime
    val window = TimeControl.computeTimeWindow(limits.moveTime)

    val legalMoves = MoveGenerator.legalMovesMutable(position)
    if legalMoves.isEmpty then return Move.None // no legal moves
    val defaultMove = MoveSorter.sortMoves(legalMoves, position).head

    val ctx = SearchContext(
      startTime = now,
      endTime = window.hardDeadline,
      depthLimit = limits.depth
    )

    @tailrec
    def iterativeDeepening(currentDepth: Depth, bestMove: Move): Move =
      val now           = SearchTime.currentTime
      val outOfTime     = now >= window.softDeadline
      val depthExceeded = currentDepth > limits.depth

      if outOfTime || depthExceeded then return bestMove

      val bestAtDepth = findBestMove(position, currentDepth, ctx)

      val after = SearchTime.currentTime
      if after >= window.hardDeadline then return bestMove

      val timeTaken = after - ctx.startTime
      val nps       = ctx.nodes.perSecond(timeTaken.toLong)
      val scoreStr  = bestAtDepth.score.format
      println(
        s"info depth $currentDepth score $scoreStr nodes ${ctx.nodes.toString} nps $nps time ${timeTaken.toString} pv ${bestAtDepth.move.toUci}"
      )

      iterativeDeepening(currentDepth + 1, bestAtDepth.move)

    if TimeControl.shouldSearch(limits.moveTime) then iterativeDeepening(Depth(1), defaultMove)
    else defaultMove

  def findBestMove(position: MutablePosition, depth: Depth, ctx: SearchContext): SearchResult =
    val moves       = MoveGenerator.pseudoLegalMovesMutable(position)
    val sortedMoves = MoveSorter.sortMoves(moves, position)

    @tailrec
    def loop(i: Int, bestMove: Move, alpha: Score, beta: Score, legalMoves: Int, ply: Ply = Ply.base): SearchResult =
      if i >= sortedMoves.size then
        if legalMoves == 0 then
          if position.isSideInCheck(position.activeSide) then SearchResult(Move.None, -Score.Checkmate)
          else SearchResult(Move.None, Score.Stalemate)
        else SearchResult(bestMove, alpha)
      else
        val move = sortedMoves(i)
        position.applyMove(move)
        if position.isSideInCheck(position.activeSide.enemy) then
          position.unapplyMove(move)
          loop(i + 1, bestMove, alpha, beta, legalMoves, ply)
        else
          ctx.nodes += 1
          val score                   = -negamax(position, depth - 1, -beta, -alpha, ctx, ply + 1)
          val (newAlpha, newBestMove) = if score > alpha then (score, move) else (alpha, bestMove)
          position.unapplyMove(move)

          loop(i + 1, newBestMove, newAlpha, beta, legalMoves + 1, ply)

    loop(0, Move.None, -Score.Infinity, Score.Infinity, 0)

  // TODO: look into maybe making this a tail recursive function
  private def negamax(position: MutablePosition, depth: Depth, alpha: Score, beta: Score, ctx: SearchContext, ply: Ply): Score =
    if shouldStop(ctx) then return Evaluation.evaluate(position)

    if depth.isZero then return quiesce(position, alpha, beta, ctx, ply)

    val moves       = MoveGenerator.pseudoLegalMovesMutable(position)
    val sortedMoves = MoveSorter.sortMoves(moves, position)

    var bestScore       = -Score.Infinity
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
      if position.isSideInCheck(position.activeSide) then -Score.Checkmate + ply.value
      else Score.Stalemate
    else bestScore

  private def quiesce(position: MutablePosition, alpha: Score, beta: Score, ctx: SearchContext, ply: Ply): Score =
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
      if legalMovesFound == 0 then -Score.Checkmate + ply.value
      else currentAlpha
    else
      val standPat = Evaluation.evaluate(position)

      if standPat >= beta then beta
      else
        var currentAlpha = Score.max(alpha, standPat)

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
    else if (ctx.nodes & 2047L).isZero && SearchTime.currentTime >= ctx.endTime then
      ctx.stopped = true
      true
    else false
