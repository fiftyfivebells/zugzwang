package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.{Color, MutablePosition, Square}
import com.ffb.zugzwang.core.{Depth, Killers, Node, Ply, Score, ScoreBuffer, SearchTime, TimeControl, TimeManager}
import com.ffb.zugzwang.evaluation.{PestoEvaluation, SEE}
import com.ffb.zugzwang.move.{Move, MoveList, MoveType}
import com.ffb.zugzwang.tools.DebugLogger

import scala.annotation.tailrec

final class Searcher:
  private val stack         = SearchStack.initialize()
  private val tt            = new TranspositionTable(256)
  private val searchHistory = new SearchHistory(stack)

  private val moveLists =
    Array.fill(Search.MaxPly.toInt + SearchConfig.qMaxDepth + 1)(MoveList(256))
  private val scoreBuffers =
    Array.fill(Search.MaxPly.toInt + SearchConfig.qMaxDepth + 1)(ScoreBuffer.initial)
  private val moveMetadata =
    Array.fill(Search.MaxPly.toInt + SearchConfig.qMaxDepth + 1)(MoveMetadata(256))

  private val timeManager = new TimeManager

  private var startTime  = SearchTime.Zero
  private var endTime    = SearchTime.Zero
  private var depthLimit = Depth(100) // TODO: make this some predefined constant value

  var nodes   = Node.Zero
  var stopped = false

  private var infoEmitted        = false
  private var lastCompletedScore = 0

  def clear(): Unit =
    tt.clear()
    stack.clear()
    searchHistory.clear()
    timeManager.clearCarryOver()

  def search(position: MutablePosition, limits: SearchLimits): Move =
    startTime = SearchTime.currentTime
    val window = TimeControl.computeTimeWindow(limits.moveTime, limits.endTime)
    endTime = window.hardDeadline
    depthLimit = limits.depth
    nodes = Node.Zero
    stopped = false
    infoEmitted = false
    SearchStats.reset()
    tt.incrementGeneration()

    val softBudgetMs =
      if window.softDeadline.isMax then Long.MaxValue / 4
      else window.softDeadline.toLong - startTime.toLong
    val hardBudgetMs =
      if window.hardDeadline.isMax then Long.MaxValue / 4
      else window.hardDeadline.toLong - startTime.toLong
    timeManager.init(softBudgetMs, hardBudgetMs, startTime.toLong)

    val rootMl = MoveList(256)
    SearchMoveGen.fillMoveList(position, rootMl)
    var legalCount = 0
    var firstLegal = Move.None
    var mi         = 0
    while mi < rootMl.size && legalCount < 2 do
      val move = rootMl.unsafeBuffer(mi)
      position.applyMove(move)
      if !position.isSideInCheck(position.activeSide.enemy) then
        legalCount += 1
        if legalCount == 1 then firstLegal = move
      position.unapplyMove(move)
      mi += 1

    if legalCount == 0 then return Move.None // checkmate or stalemate

    // Single legal move: no need to search, return it immediately
    if legalCount == 1 then
      val timeTaken = SearchTime.currentTime - startTime
      println(s"info depth 1 score cp 0 nodes 1 nps 0 time ${timeTaken.toString} pv ${firstLegal.toUci}")
      return firstLegal

    @tailrec
    def iterativeDeepening(
      position: MutablePosition,
      currentDepth: Depth,
      bestMove: Move,
      prevScore: Score
    ): Move =
      if currentDepth > Depth(1) then
        val now = SearchTime.currentTime
        if !timeManager.shouldContinue(bestMove.value, prevScore.toInt, currentDepth.toInt - 1, now.toLong, 0.0) || stopped then
          return bestMove
      if currentDepth > depthLimit || stopped then return bestMove

      var alpha          = if currentDepth >= Depth(SearchConfig.aspMinDepth) then prevScore - SearchConfig.aspWindowSize else -Score.Infinity
      var beta           = if currentDepth >= Depth(SearchConfig.aspMinDepth) then prevScore + SearchConfig.aspWindowSize else Score.Infinity
      var delta          = SearchConfig.aspWindowSize
      var attempts       = 0
      var score          = Score.Zero
      var searchComplete = false

      while !searchComplete do
        score = negamax(position, currentDepth, alpha, beta, Ply.Base)

        if SearchTime.currentTime >= endTime || stopped then return bestMove

        if (score <= alpha || score >= beta) && attempts < SearchConfig.aspMaxAttempts then
          attempts += 1
          delta = delta * 2

          if score <= alpha then
            SearchStats.aspirationFailLows += 1
            alpha = Score.max(alpha - delta, -Score.Infinity)

          if score >= beta then
            SearchStats.aspirationFailHighs += 1
            beta = Score.min(beta + delta, Score.Infinity)
        else searchComplete = true

      val rootEntry    = stack.at(Ply.Base)
      val nextBestMove = if rootEntry.bestMove != Move.None then rootEntry.bestMove else bestMove

      val timeTaken  = SearchTime.currentTime - startTime
      val totalNodes = Node(SearchStats.nodes + SearchStats.qNodes)
      val nps        = totalNodes.perSecond(timeTaken.toLong)
      infoEmitted = true
      println(
        s"info depth $currentDepth score ${score.format} nodes ${totalNodes.toString} nps $nps time ${timeTaken.toString} pv ${nextBestMove.toUci}"
      )
      lastCompletedScore = score.toInt

      iterativeDeepening(position, currentDepth + 1, nextBestMove, score)

    val result =
      try iterativeDeepening(position, Depth(1), firstLegal, Score.Draw)
      catch
        case e =>
          DebugLogger.log("CRASH")
          DebugLogger.log(e.getMessage())
          DebugLogger.log(e.getStackTrace().mkString("\n"))
          firstLegal

    timeManager.onSearchComplete(lastCompletedScore)

    if !infoEmitted then
      val timeTaken  = SearchTime.currentTime - startTime
      val totalNodes = Node(SearchStats.nodes + SearchStats.qNodes)
      println(s"info depth 0 score cp 0 nodes ${totalNodes.toString} nps 0 time ${timeTaken.toString} pv ${result.toUci}")

    if result.isNoMove then firstLegal else result

  private inline def nullMoveReduction(depth: Depth): Int =
    if depth > SearchConfig.nmpDeepThreshold then SearchConfig.nmpDeepReduction
    else SearchConfig.nmpBaseReduction

  private def attemptNullMove(
    position: MutablePosition,
    depth: Depth,
    beta: Score,
    ply: Ply
  ): Boolean =
    if depth.toInt < SearchConfig.nmpMinDepth ||
      ply.toInt == 0 ||
      beta >= Score.Infinity ||
      !position.hasMajorPieces(position.activeSide)
    then false
    else
      position.applyNullMove
      try
        val score = -negamax(position, depth - 1 - nullMoveReduction(depth), -beta, -beta + 1, ply + 1)
        score >= beta
      finally position.unapplyNullMove

  private inline def computeReduction(depth: Depth, moveIndex: Int): Depth =
    if depth.toInt < SearchConfig.lmrMinDepth || moveIndex < SearchConfig.lmrMinMoveIndex then Depth.Zero
    else
      val r = SearchConfig.lmrTable(math.min(depth.toInt, 127))(math.min(moveIndex, 127))
      Depth(math.min(r, depth.toInt - 1))

  // decides whether a move is eligible for LMR. call this after applyMove
  private def isReducible(
    position: MutablePosition,
    move: Move,
    killers: Killers,
    inCheck: Boolean
  ): Boolean =
    val givesCheck = position.isSideInCheck(position.activeSide)
    !inCheck && !givesCheck &&
    !move.isCapture && !move.isPromotion &&
    !killers.doesContain(move)

  private def tryReverseFutility(
    isPvNode: Boolean,
    isRootNode: Boolean,
    depth: Depth,
    inCheck: Boolean,
    staticEval: Score,
    beta: Score
  ): Score =
    if !isPvNode && !isRootNode && depth <= Depth(SearchConfig.rfpMaxDepth) && !inCheck then
      val mateGuard = Score.Checkmate - Search.MaxPly.toInt
      if beta < mateGuard && beta > -mateGuard &&
        staticEval - SearchConfig.rfpMarginPerDepth * depth.toInt >= beta
      then
        SearchStats.rfpPrunes += 1
        return staticEval
    Score.NoScore

  private def tryRazoring(
    position: MutablePosition,
    isPvNode: Boolean,
    isRootNode: Boolean,
    depth: Depth,
    inCheck: Boolean,
    staticEval: Score,
    alpha: Score,
    ply: Ply
  ): Score =
    if !isPvNode && !isRootNode && depth <= Depth(SearchConfig.razorMaxDepth) && !inCheck then
      val margin = if depth == Depth(1) then SearchConfig.razorMarginD1 else SearchConfig.razorMarginD2
      if staticEval + margin < alpha then
        val qScore = quiesce(position, alpha - 1, alpha, ply)
        SearchStats.razorProbes += 1
        if qScore < alpha then
          SearchStats.razorPrunes += 1
          return qScore
    Score.NoScore

  private def negamax(
    position: MutablePosition,
    depth: Depth,
    alpha: Score,
    beta: Score,
    ply: Ply
  ): Score =
    SearchStats.nodes += 1
    nodes = nodes + 1

    val isPvNode   = beta - alpha > 1
    val isRootNode = ply == Ply.Base

    val currEntry = stack.at(ply)
    currEntry.quietsTried.clear()
    currEntry.capturesTried.clear()
    currEntry.bestMove = Move.None
    currEntry.isPvNode = isPvNode

    // early returns
    if ply >= Search.MaxPly then return PestoEvaluation.evaluate(position)
    if position.halfMoveClock >= 100 then return Score.Draw
    if ply > 0 && position.isRepetition then return Score.Draw

    SearchStats.ttProbes += 1
    val ttEntry = tt.probe(position.zobristHash)
    var ttMove  = Move.None

    if ttEntry.isDefined then
      SearchStats.ttHits += 1
      ttMove = ttEntry.move
      // A castle move cached when the rook existed may be replayed after the rook
      // is captured, bypassing SearchMoveGen's rook-presence check. Discard it.
      if ttMove.moveType == MoveType.CastleKingside || ttMove.moveType == MoveType.CastleQueenside then
        val rookSq =
          if ttMove.moveType == MoveType.CastleKingside then if position.activeSide == Color.White then Square.H1 else Square.H8
          else if position.activeSide == Color.White then Square.A1
          else Square.A8
        if position.pieceAt(rookSq).isNoPiece then ttMove = Move.None
      if !isPvNode && ply.toInt > 0 && ttEntry.canCutoff(depth, alpha, beta, ply) then return ttEntry.score(ply)

    val newDepth = if ttMove == Move.None && depth >= Depth(SearchConfig.iirMinDepth) then
      SearchStats.iirReductions += 1
      depth - 1
    else depth

    val inCheck = position.isSideInCheck(position.activeSide)
    currEntry.inCheck = inCheck

    if shouldStop() then return PestoEvaluation.evaluate(position)

    // null move pruning
    if !isPvNode && !isRootNode && !inCheck && attemptNullMove(position, newDepth, beta, ply) then return beta

    // check extension
    val extension = if inCheck && (!SearchConfig.checkExtPvOnly || isPvNode) then Depth(1) else Depth.Zero

    // TODO: future feature: singular extensions:
    // if ttMove exists, depth >= 8, and tt entry has enough depth:
    // -- set currentEntry.excudedMove = ttMove
    // -- do a reduced search, only extend ttMove if result + margin < ttScore

    val searchDepth = newDepth + extension

    if searchDepth.isZero then
      SearchStats.leafNodes += 1
      return quiesce(position, alpha, beta, ply)

    val staticEval = if !inCheck then PestoEvaluation.evaluate(position) else Score.Zero
    currEntry.staticEval = staticEval

    // TODO: future feature: improving detecion
    // val improving = ply > 1 && Score.isDefined(staticEval) &&
    //   Score.isDefined(stack(ply - 2).staticEval) &&
    //   staticEval > stack(ply - 2).staticEval
    // use this to adjust rfp margins and lmr aggressiveness

    val rfp = tryReverseFutility(isPvNode, isRootNode, newDepth, inCheck, staticEval, beta)
    if rfp != Score.NoScore then return rfp

    val razor = tryRazoring(position, isPvNode, isRootNode, newDepth, inCheck, staticEval, alpha, ply)
    if razor != Score.NoScore then return razor

    val canDoFutility =
      !isPvNode && !isRootNode && newDepth <= Depth(
        SearchConfig.fpMaxDepth
      ) && !inCheck && staticEval + newDepth.toInt * SearchConfig.fpMarginPerDepth <= alpha

    val moveBuf = moveLists(ply.toInt)
    SearchMoveGen.fillMoveList(position, moveBuf)
    val moveArr   = moveBuf.unsafeBuffer
    val moveCount = moveBuf.size
    val scoreArr  = scoreBuffers(ply.toInt)
    val meta      = moveMetadata(ply.toInt)

    val currentKillers = searchHistory.killersAtPly(ply)

    MoveSorter.sortMoves(
      moveArr,
      scoreArr,
      meta,
      moveCount,
      position,
      searchHistory,
      position.activeSide.ordinal,
      ttMove,
      ply
    )

    var bestScore          = -Score.Infinity
    var bestMove           = Move.None
    var currentAlpha       = alpha
    var legalMovesFound    = 0
    var quietMovesSearched = 0
    var ttFlag             = TTEntry.FlagUpper

    var i = 0
    while i < moveCount do
      val move = MoveSorter.pickNext(moveArr, scoreArr, meta, i, moveCount)

      if canDoFutility &&
        !meta.isCapture(i) &&
        !meta.isPromotion(i) &&
        move != ttMove &&
        legalMovesFound > 0
      then
        SearchStats.futilityPrunes += 1
        i += 1
      else if !isPvNode &&
        searchDepth <= Depth(SearchConfig.lmpMaxDepth) &&
        !inCheck &&
        meta.isQuiet(i) &&
        !currentKillers.doesContain(move) &&
        move != ttMove &&
        quietMovesSearched >= SearchConfig.lmpThresholds(searchDepth.toInt)
      then
        SearchStats.lmpPrunes += 1
        i += 1
      else
        position.applyMove(move)

        if !position.isSideInCheck(position.activeSide.enemy) then
          legalMovesFound += 1

          if meta.isQuiet(i) then
            quietMovesSearched += 1
            currEntry.addQuiet(move)
          else if meta.isCapture(i) then currEntry.addCapture(move)

          val reduction =
            if searchDepth.toInt >= SearchConfig.lmrMinDepth &&
              i >= SearchConfig.lmrMinMoveIndex &&
              isReducible(position, move, currentKillers, inCheck)
            then computeReduction(searchDepth, i)
            else Depth.Zero

          var score = Score.Zero

          if reduction > Depth.Zero then
            SearchStats.lmrReductions += 1
            val movedPiece = position.pieceAt(move.to)
            val histScore  = searchHistory.quietScore(movedPiece, move.to)
            val histAdj    = histScore / SearchConfig.lmrHistoryDivisor

            val finalReduction = Depth(math.max(1, (reduction.toInt - histAdj.toInt)))

            // Stage 1: reduced depth, null window
            score = -negamax(position, searchDepth - 1 - finalReduction, -currentAlpha - 1, -currentAlpha, ply + 1)

            if score > currentAlpha then
              SearchStats.lmrResearches += 1
              // Stage 2: full depth, null window
              score = -negamax(position, searchDepth - 1, -currentAlpha - 1, -currentAlpha, ply + 1)
              if score > currentAlpha then
                // Stage 3: full depth, full window (only on PV)
                score = -negamax(position, searchDepth - 1, -beta, -currentAlpha, ply + 1)
          else if legalMovesFound > 1 then
            // Non-LMR, non-first move: PVS null window first, then full window re-search only at PV nodes
            score = -negamax(position, searchDepth - 1, -currentAlpha - 1, -currentAlpha, ply + 1)
            if score > currentAlpha && isPvNode then
              SearchStats.pvsReSearches += 1
              score = -negamax(position, searchDepth - 1, -beta, -currentAlpha, ply + 1)
          else
            // First legal move: full window search directly
            score = -negamax(position, searchDepth - 1, -beta, -currentAlpha, ply + 1)

          position.unapplyMove(move)

          if score > bestScore then
            bestMove = move
            bestScore = score

          if score >= beta then
            SearchStats.betaCutoffs += 1
            tt.store(position.zobristHash, move, beta, searchDepth, TTEntry.FlagLower, ply)

            if i == 0 then SearchStats.firstMoveCutoffs += 1
            else if searchHistory.killersAtPly(ply).doesContain(move) then SearchStats.killerCutoffs += 1
            else SearchStats.historyCutoffs += 1

            if meta.isQuiet(i) then searchHistory.updateAfterQuietCutoff(position, ply, move, newDepth)
            else if meta.isCapture(i) then searchHistory.updateAfterCaptureCutoff(position, ply, move, newDepth)

            return beta

          if score > currentAlpha then
            currentAlpha = score
            ttFlag = TTEntry.FlagExact
            currEntry.bestMove = move
        else position.unapplyMove(move) // illegal move

        i += 1

    if stopped then return currentAlpha

    if legalMovesFound == 0 then
      // TODO: future feature: mate distance pruning
      // alpha = max(alpha, -Score.Checkmate + ply)
      // beta = min(beta, Score.Checkmate - ply)
      // if alpha >= beta then return alpha

      if position.isSideInCheck(position.activeSide) then -Score.Checkmate + ply.toInt
      else Score.Stalemate
    else
      tt.store(position.zobristHash, bestMove, bestScore, newDepth, ttFlag, ply)
      bestScore

  private def quiesce(position: MutablePosition, alpha: Score, beta: Score, ply: Ply, qDepth: Int = 0): Score =
    SearchStats.qNodes += 1
    nodes = nodes + 1
    SearchStats.qSearchMaxDepth = Math.max(qDepth, SearchStats.qSearchMaxDepth)

    if qDepth >= SearchConfig.qMaxDepth then return PestoEvaluation.evaluate(position)

    SearchStats.qTtProbes += 1
    val ttEntry = tt.probe(position.zobristHash)
    if ttEntry.isDefined then
      SearchStats.qTtHits += 1
      if ttEntry.canCutoff(Depth.Zero, alpha, beta, ply) then return ttEntry.score(ply)

    if shouldStop() then return PestoEvaluation.evaluate(position)

    if position.isSideInCheck(position.activeSide) then
      // Stand-pat is invalid when in check — must search all evasions
      val moveBuf = moveLists(ply.toInt)
      SearchMoveGen.fillMoveList(position, moveBuf)
      val moveArr      = moveBuf.unsafeBuffer
      val moveCount    = moveBuf.size
      var bestScore    = -Score.Infinity
      var currentAlpha = alpha
      var legalMoves   = 0

      var i = 0
      while i < moveCount && !shouldStop() do
        val move = moveArr(i)
        position.applyMove(move)
        if !position.isSideInCheck(position.activeSide.enemy) then
          legalMoves += 1
          val score = -quiesce(position, -beta, -currentAlpha, ply + 1, qDepth + 1)
          position.unapplyMove(move)
          if score >= beta then return beta
          if score > bestScore then bestScore = score
          if score > currentAlpha then currentAlpha = score
        else position.unapplyMove(move)
        i += 1

      if stopped then return bestScore
      if legalMoves == 0 then return -Score.Checkmate + ply.toInt
      bestScore
    else
      val standPat = PestoEvaluation.evaluate(position)
      if standPat >= beta then return beta

      var currentAlpha = Score.max(alpha, standPat)

      val captureBuf = moveLists(ply.toInt)
      SearchMoveGen.fillCaptures(position, captureBuf)
      val captureArr   = captureBuf.unsafeBuffer
      val captureCount = captureBuf.size
      val scoreArr     = scoreBuffers(ply.toInt)
      MoveSorter.scoreCaptures(captureArr, scoreArr, captureCount, position)
      SearchStats.qSearchCapturesGenerated += captureCount

      var i = 0
      while i < captureCount && !shouldStop() do
        val move     = MoveSorter.pickNext(captureArr, scoreArr, null, i, captureCount)
        val captured = position.pieceAt(move.to)

        if standPat + captured.materialValue + SearchConfig.qFutilityMargin >= currentAlpha then
          if SEE.seeGE(position, move) then
            position.applyMove(move)
            if !position.isSideInCheck(position.activeSide.enemy) then
              val score = -quiesce(position, -beta, -currentAlpha, ply + 1, qDepth + 1)
              position.unapplyMove(move)
              if score >= beta then return beta
              if score > currentAlpha then currentAlpha = score
            else position.unapplyMove(move)
          else SearchStats.seePrunesQSearch += 1

        SearchStats.qSearchMovesSearched += 1
        i += 1

      currentAlpha

  private inline def shouldStop(): Boolean =
    if stopped then true
    else if (nodes & 2047L).isZero && SearchTime.currentTime >= endTime then
      stopped = true
      true
    else false
