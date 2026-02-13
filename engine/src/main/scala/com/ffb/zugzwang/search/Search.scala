package com.ffb.zugzwang.search
import com.ffb.zugzwang.chess.MutablePosition
import com.ffb.zugzwang.core.{Depth, Node, Ply, Score, SearchTime, TimeControl}
import com.ffb.zugzwang.evaluation.PestoEvaluation
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
  var stopped: Boolean = false,
  val killers: Array[Array[Move]] = Array.fill(Search.MaxPly.value, 2)(Move.None),
  val history: Array[Array[Score]] = Array.ofDim[Score](64, 64)
):

  def storeKiller(ply: Ply, move: Move): Unit =
    if ply >= Search.MaxPly then return

    val p = ply.value

    if killers(p)(0) == move then return

    killers(p)(1) = killers(p)(0)
    killers(p)(0) = move

  def updateHistory(move: Move, depth: Depth): Unit =
    val bonus   = depth.value * depth.value
    val current = history(move.from.value)(move.to.value)
    if current < 50000 then                                                // TODO: magic number alert, make this a constant
      history(move.from.value)(move.to.value) = current * 95 / 100 + bonus // 5% decay

object Search:
  val MaxPly     = Ply(128)
  private val tt = new TranspositionTable(256)

  def clear: Unit = tt.clear()

  def search(position: MutablePosition, limits: SearchLimits): Move =
    val now    = SearchTime.currentTime
    val window = TimeControl.computeTimeWindow(limits.moveTime)

    val legalMoves = MoveGenerator.legalMovesMutable(position)
    if legalMoves.isEmpty then return Move.None // no legal moves

    val ctx = SearchContext(
      startTime = now,
      endTime = window.hardDeadline,
      depthLimit = limits.depth,
      table = tt
    )

    val defaultMove = MoveSorter.sortMoves(legalMoves, position, ctx.killers(0), ctx.history).head

    @tailrec
    def iterativeDeepening(currentDepth: Depth, bestMove: Move, prevScore: Score): Move =
      val now           = SearchTime.currentTime
      val outOfTime     = now >= window.softDeadline
      val depthExceeded = currentDepth > limits.depth

      if outOfTime || depthExceeded then return bestMove

      val (alpha, beta) =
        if currentDepth >= Depth(5) then
          val windowSize = 50 // TODO: maybe experiment with window sizes?
          (prevScore - windowSize, prevScore + windowSize)
        else (-Score.Infinity, Score.Infinity)

      val result = findBestMoveWithAspiration(position, currentDepth, alpha, beta, ctx)

      val after = SearchTime.currentTime
      if after >= window.hardDeadline then return bestMove

      val timeTaken  = after - ctx.startTime
      val totalNodes = Node(SearchStats.nodes + SearchStats.qNodes)
      val nps        = totalNodes.perSecond(timeTaken.toLong)
      val scoreStr   = result.score.format
      println(
        s"info depth $currentDepth score $scoreStr nodes ${totalNodes.toString} nps $nps time ${timeTaken.toString} pv ${result.move.toUci}"
      )

      val nextBestMove = if result.move != Move.None then result.move else bestMove
      iterativeDeepening(currentDepth + 1, nextBestMove, result.score)

    if TimeControl.shouldSearch(limits.moveTime) then iterativeDeepening(Depth(1), defaultMove, Score.Draw)
    else defaultMove

  def findBestMoveWithAspiration(
    position: MutablePosition,
    depth: Depth,
    initialAlpha: Score,
    initialBeta: Score,
    ctx: SearchContext
  ): SearchResult =
    var alpha = initialAlpha
    var beta  = initialBeta

    var result = findBestMove(position, depth, alpha, beta, ctx)

    var attempts    = 0
    val maxAttempts = 3

    while (result.score <= alpha || result.score >= beta) && attempts < maxAttempts do
      attempts += 1

      if result.score <= alpha then
        SearchStats.aspirationFailLows += 1
        alpha = alpha - (100 * attempts)
        if alpha < -Score.Infinity then alpha = -Score.Infinity

      if result.score >= beta then
        SearchStats.aspirationFailHighs += 1
        beta = beta + (100 * attempts)
        if beta > Score.Infinity then beta = Score.Infinity

      result = findBestMove(position, depth, alpha, beta, ctx)

    result

  def findBestMove(position: MutablePosition, depth: Depth, alpha: Score, beta: Score, ctx: SearchContext): SearchResult =
    val ttEntry = ctx.table.probe(position.zobristHash)
    val ttMove  = if ttEntry.isDefined then ttEntry.move else Move.None

    val moves       = MoveGenerator.pseudoLegalMovesMutable(position)
    val sortedMoves = MoveSorter.sortMoves(moves, position, ctx.killers(0), ctx.history, ttMove)

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
          val rawScore          = -negamax(position, depth - 1, -beta, -alpha, ctx, ply + 1)
          val comparisonScore =
            if rawScore == Score.Draw then
              alpha match
                case a if a < Score.Draw => rawScore + Score.DrawBias
                case a if a > Score.Draw => rawScore - Score.DrawBias
                case _                   => rawScore
            else rawScore

          val (newAlpha, newBestMove) = if comparisonScore > alpha then (rawScore, move) else (alpha, bestMove)
          position.unapplyMove(move)

          loop(i + 1, newBestMove, newAlpha, beta, legalMoves + 1, newFirstLegalMove, ply)

    loop(0, Move.None, alpha, beta, 0)

  private inline def nullMoveReduction(depth: Depth): Int = if depth > 6 then 3 else 2

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
        val score = -negamax(position, depth - 1 - nullMoveReduction(depth), -beta, -beta + 1, ctx, ply + 1)
        score >= beta
      finally position.unapplyNullMove

  // TODO: look into maybe making this a tail recursive function
  private def negamax(position: MutablePosition, depth: Depth, alpha: Score, beta: Score, ctx: SearchContext, ply: Ply): Score =
    SearchStats.nodes += 1
    ctx.nodes = ctx.nodes + 1

    if position.halfMoveClock >= 100 then return Score.Draw
    if ply > 0 && position.isRepetition then return Score.Draw

    val ttEntry = ctx.table.probe(position.zobristHash)
    var ttMove  = Move.None

    if ttEntry.isDefined then
      SearchStats.ttHits += 1
      ttMove = ttEntry.move
      if ply.value > 0 && ttEntry.canCutoff(depth, alpha, beta, ply) then return ttEntry.score(ply)

    if attemptNullMove(position, depth, beta, ctx, ply) then return beta

    if shouldStop(ctx) then return PestoEvaluation.evaluate(position)
    if depth.isZero then
      SearchStats.leafNodes += 1
      return quiesce(position, alpha, beta, ctx, ply)

    val moves = MoveGenerator.pseudoLegalMovesMutable(position)

    val currentKillers = if ply < MaxPly then ctx.killers(ply.value) else Array.empty[Move]
    val sortedMoves    = MoveSorter.sortMoves(moves, position, currentKillers, ctx.history, ttMove)

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
        legalMovesFound += 1

        val score = -negamax(position, depth - 1, -beta, -currentAlpha, ctx, ply + 1)

        position.unapplyMove(move)

        if score >= beta then
          SearchStats.betaCutoffs += 1
          ctx.table.store(position.zobristHash, move, beta, depth, TTEntry.FlagLower, ply)

          if i == 0 then SearchStats.firstMoveCutoffs += 1
          else if currentKillers.contains(move) then SearchStats.killerCutoffs += 1
          else SearchStats.historyCutoffs += 1

          if !move.isCapture then
            ctx.storeKiller(ply, move)
            ctx.updateHistory(move, depth)

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

  private def quiesce(position: MutablePosition, alpha: Score, beta: Score, ctx: SearchContext, ply: Ply, qDepth: Int = 0): Score =
    SearchStats.qNodes += 1
    ctx.nodes = ctx.nodes + 1
    SearchStats.qSearchMaxDepth = Math.max(qDepth, SearchStats.qSearchMaxDepth)

    val MaxQDepth = 10
    if qDepth >= MaxQDepth then return PestoEvaluation.evaluate(position)

    if shouldStop(ctx) then PestoEvaluation.evaluate(position)
    else if position.isSideInCheck(position.activeSide) then
      val moves           = MoveGenerator.pseudoLegalMovesMutable(position)
      var bestScore       = -Score.Infinity
      var legalMovesFound = 0

      var i = 0
      while i < moves.size && !shouldStop(ctx) do
        val move = moves(i)
        position.applyMove(move)

        // after applyMove, side-to-move flips; check legality by ensuring the mover's king isn't in check
        if !position.isSideInCheck(position.activeSide.enemy) then
          SearchStats.qSearchInCheckCount += 1
          legalMovesFound += 1

          val score = -quiesce(position, -beta, -alpha, ctx, ply, qDepth + 1)

          position.unapplyMove(move)

          if score >= beta then return beta
          if score > bestScore then bestScore = score
        else position.unapplyMove(move)

        i += 1

      if ctx.stopped then return bestScore

      // Checkmated (mate score is ply-based)
      if legalMovesFound == 0 then -Score.Checkmate + ply.value
      else bestScore
    else
      val standPat = PestoEvaluation.evaluate(position)

      if standPat >= beta then beta
      else
        var currentAlpha = Score.max(alpha, standPat)

        val DeltaMargin = 900
        if standPat + DeltaMargin < alpha then return currentAlpha

        val captures       = MoveGenerator.pseudoLegalCapturesMutable(position)
        val sortedCaptures = MoveSorter.sortCaptures(captures.toArray, position)
        SearchStats.qSearchCapturesGenerated += captures.size

        var i = 0
        while i < sortedCaptures.size && !shouldStop(ctx) do
          val move = sortedCaptures(i)

          val captured = position.pieceAt(move.to)
          if standPat + captured.pieceType.value + 200 < alpha then i += 1
          else
            position.applyMove(move)

            if !position.isSideInCheck(position.activeSide.enemy) then
              val score = -quiesce(position, -beta, -currentAlpha, ctx, ply, qDepth + 1)

              position.unapplyMove(move)

              if score >= beta then return beta
              if score > currentAlpha then currentAlpha = score
            else position.unapplyMove(move)

          i += 1
          SearchStats.qSearchMovesSearched += 1
        currentAlpha

  private inline def shouldStop(ctx: SearchContext): Boolean =
    if ctx.stopped then true
    else if (ctx.nodes & 2047L).isZero && SearchTime.currentTime >= ctx.endTime then
      ctx.stopped = true
      true
    else false
