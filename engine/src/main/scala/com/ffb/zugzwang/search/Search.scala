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
  val table: TranspositionTable,
  var nodes: Node = Node.zero,
  var stopped: Boolean = false
)

object Search:
  private val tt = new TranspositionTable(64)

  def clear: Unit = tt.clear()

  def search(position: MutablePosition, limits: SearchLimits): Move =
    val now    = SearchTime.currentTime
    val window = TimeControl.computeTimeWindow(limits.moveTime)

    val legalMoves = MoveGenerator.legalMovesMutable(position)
    if legalMoves.isEmpty then return Move.None // no legal moves
    val defaultMove = MoveSorter.sortMoves(legalMoves, position).head

    val ctx = SearchContext(
      startTime = now,
      endTime = window.hardDeadline,
      depthLimit = limits.depth,
      table = tt
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

      val nextBestMove = if bestAtDepth.move != Move.None then bestAtDepth.move else bestMove
      iterativeDeepening(currentDepth + 1, nextBestMove)

    if TimeControl.shouldSearch(limits.moveTime) then iterativeDeepening(Depth(1), defaultMove)
    else defaultMove

  def findBestMove(position: MutablePosition, depth: Depth, ctx: SearchContext): SearchResult =
    val ttEntry = ctx.table.probe(position.zobristHash)
    val ttMove  = if ttEntry.isDefined then ttEntry.move else Move.None

    val moves       = MoveGenerator.pseudoLegalMovesMutable(position)
    val sortedMoves = MoveSorter.sortMoves(moves, position, ttMove)

    @tailrec
    def loop(
      i: Int,
      bestMove: Move,
      alpha: Score,
      beta: Score,
      legalMoves: Int,
      firstLegalMove: Move = Move.None,
      ply: Ply = Ply.base
    ): SearchResult =
      if i >= sortedMoves.size then
        if legalMoves == 0 then
          if position.isSideInCheck(position.activeSide) then SearchResult(Move.None, -Score.Checkmate)
          else SearchResult(Move.None, Score.Stalemate)
        else
          val finalMove = if bestMove == Move.None then firstLegalMove else bestMove
          ctx.table.store(position.zobristHash, finalMove, alpha, depth, TTEntry.FlagExact, ply)
          SearchResult(finalMove, alpha)
      else
        val move = sortedMoves(i)
        position.applyMove(move)
        if position.isSideInCheck(position.activeSide.enemy) then
          position.unapplyMove(move)
          loop(i + 1, bestMove, alpha, beta, legalMoves, firstLegalMove, ply)
        else
          val newFirstLegalMove = if firstLegalMove == Move.None then move else firstLegalMove
          ctx.nodes += 1
          val score                   = -negamax(position, depth - 1, -beta, -alpha, ctx, ply + 1)
          val (newAlpha, newBestMove) = if score > alpha then (score, move) else (alpha, bestMove)
          position.unapplyMove(move)

          loop(i + 1, newBestMove, newAlpha, beta, legalMoves + 1, newFirstLegalMove, ply)

    loop(0, Move.None, -Score.Infinity, Score.Infinity, 0)

  private inline val NullMoveReduction = 2

  private def attemptNullMove(
    position: MutablePosition,
    depth: Depth,
    beta: Score,
    ctx: SearchContext,
    ply: Ply
  ): Boolean =
    if depth.value < 3 ||
      position.isSideInCheck(position.activeSide) ||
      ply.value == 0 ||
      beta >= Score.Infinity ||
      !position.hasMajorPieces(position.activeSide)
    then false
    else
      try
        position.applyNullMove
        val score = -negamax(position, depth - 1 - NullMoveReduction, -beta, -beta + 1, ctx, ply + 1)
        score >= beta
      finally position.unapplyNullMove

  // TODO: look into maybe making this a tail recursive function
  private def negamax(position: MutablePosition, depth: Depth, alpha: Score, beta: Score, ctx: SearchContext, ply: Ply): Score =
    if ply > 0 && (position.isRepetition || position.halfMoveClock >= 100) then return Score.Draw

    val ttEntry = ctx.table.probe(position.zobristHash)
    var ttMove  = Move.None

    if ttEntry.isDefined then
      ttMove = ttEntry.move
      if ply.value > 0 && ttEntry.canCutoff(depth, alpha, beta, ply) then return ttEntry.score(ply)

    if attemptNullMove(position, depth, beta, ctx, ply) then return beta

    if shouldStop(ctx) then return Evaluation.evaluate(position)
    if depth.isZero then return quiesce(position, alpha, beta, ctx, ply)

    val moves       = MoveGenerator.pseudoLegalMovesMutable(position)
    val sortedMoves = MoveSorter.sortMoves(moves, position)

    var bestScore       = -Score.Infinity
    var bestMove        = Move.None // for tracking TT move
    var currentAlpha    = alpha
    var legalMovesFound = 0
    var ttFlag          = TTEntry.FlagUpper

    var i = 0
    while i < sortedMoves.size do
      val move = sortedMoves(i)

      position.applyMove(move)

      if !position.isSideInCheck(position.activeSide.enemy) then
        ctx.nodes += 1
        legalMovesFound += 1

        val score = -negamax(position, depth - 1, -beta, -currentAlpha, ctx, ply + 1)

        position.unapplyMove(move)

        if score >= beta then
          ctx.table.store(position.zobristHash, move, beta, depth, TTEntry.FlagLower, ply)
          return beta
        if score > bestScore then
          bestMove = move
          bestScore = score
        if score > currentAlpha then
          currentAlpha = score
          ttFlag = TTEntry.FlagExact
      else position.unapplyMove(move) // illegal move

      i += 1

    if ctx.stopped then return currentAlpha

    if legalMovesFound == 0 then
      if position.isSideInCheck(position.activeSide) then -Score.Checkmate + ply.value
      else Score.Stalemate
    else
      ctx.table.store(position.zobristHash, bestMove, bestScore, depth, ttFlag, ply)
      bestScore

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
