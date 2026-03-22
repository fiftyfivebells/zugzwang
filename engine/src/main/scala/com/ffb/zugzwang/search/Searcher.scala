package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.MutablePosition
import com.ffb.zugzwang.core.{Depth, Node, Ply, Score, ScoreBuffer, SearchTime, TimeControl}
import com.ffb.zugzwang.evaluation.{PestoEvaluation, SEE}
import com.ffb.zugzwang.move.{Move, MoveList}
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

  private var startTime  = SearchTime.Zero
  private var endTime    = SearchTime.Zero
  private var depthLimit = Depth(100) // TODO: make this some predfined constant value

  var nodes   = Node.Zero
  var stopped = false

  def clear(): Unit =
    tt.clear()
    stack.clear()
    searchHistory.clear()

  def search(position: MutablePosition, limits: SearchLimits): Move =
    startTime = SearchTime.currentTime
    val window = TimeControl.computeTimeWindow(limits.moveTime)
    endTime = window.hardDeadline
    depthLimit = limits.depth
    nodes = Node.Zero
    stopped = false
    SearchStats.reset()
    tt.incrementGeneration()

    if !TimeControl.shouldSearch(limits.moveTime) then return Move.None

    val rootMl = MoveList(256)
    SearchMoveGen.fillMoveList(position, rootMl)
    var legalCount = 0
    var onlyLegal  = Move.None
    var mi         = 0
    while mi < rootMl.size && legalCount < 2 do
      val move = rootMl.unsafeBuffer(mi)
      position.applyMove(move)
      if !position.isSideInCheck(position.activeSide.enemy) then
        legalCount += 1
        onlyLegal = move
      position.unapplyMove(move)
      mi += 1

    if legalCount == 0 then return Move.None
    if legalCount == 1 then return onlyLegal

    @tailrec
    def iterativeDeepening(
      position: MutablePosition,
      currentDepth: Depth,
      bestMove: Move,
      prevScore: Score,
      softDeadline: SearchTime
    ): Move =
      val now = SearchTime.currentTime
      if now >= softDeadline || currentDepth > depthLimit || stopped then return bestMove

      var alpha          = if currentDepth >= Depth(SearchConfig.aspMinDepth) then prevScore - SearchConfig.aspWindowSize else -Score.Infinity
      var beta           = if currentDepth >= Depth(SearchConfig.aspMinDepth) then prevScore + SearchConfig.aspWindowSize else Score.Infinity
      var delta          = SearchConfig.aspWindowSize
      var attempts       = 0
      var score          = Score.Zero
      var searchComplete = false

      while !searchComplete do
        score = negamax(position, currentDepth, alpha, beta, Ply.Base)

        if SearchTime.currentTime >= endTime || stopped then
          val rootBest = stack.at(Ply.Base).bestMove
          return if rootBest != Move.None then rootBest else bestMove

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
      println(
        s"info depth $currentDepth score ${score.format} nodes ${totalNodes.toString} nps $nps time ${timeTaken.toString} pv ${nextBestMove.toUci}"
      )

      iterativeDeepening(position, currentDepth + 1, nextBestMove, score, softDeadline)

    try iterativeDeepening(position, Depth(1), Move.None, Score.Draw, window.softDeadline)
    catch
      case e =>
        DebugLogger.log("CRASH")
        DebugLogger.log(e.getMessage())
        DebugLogger.log(e.getStackTrace().mkString("\n"))
        Move.None

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
      position.isSideInCheck(position.activeSide) ||
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

  private inline def shouldReduce(
    position: MutablePosition,
    move: Move,
    moveIndex: Int,
    depth: Depth,
    ply: Ply
  ): Boolean =
    val inCheck        = position.isSideInCheck(position.activeSide.enemy)
    val givesCheck     = position.isSideInCheck(position.activeSide)
    val isCapture      = move.isCapture
    val isPromotion    = move.isPromotion
    val currentKillers = searchHistory.killersAtPly(ply)
    val isKiller       = currentKillers.doesContain(move)
    val isGoodCapture  = isCapture && SEE.seeGE(position, move)

    depth >= Depth(SearchConfig.lmrMinDepth) &&
    moveIndex >= SearchConfig.lmrMinMoveIndex &&
    !inCheck &&
    !givesCheck &&
    !isCapture &&
    !isPromotion &&
    !isKiller &&
    !isGoodCapture

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

    if ply >= Search.MaxPly then return PestoEvaluation.evaluate(position)

    if position.halfMoveClock >= 100 then return Score.Draw
    if ply > 0 && position.isRepetition then return Score.Draw

    SearchStats.ttProbes += 1
    val ttEntry = tt.probe(position.zobristHash)
    var ttMove  = Move.None

    if ttEntry.isDefined then
      SearchStats.ttHits += 1
      ttMove = ttEntry.move
      if !isPvNode && ply.toInt > 0 && ttEntry.canCutoff(depth, alpha, beta, ply) then return ttEntry.score(ply)

    // internal iterative reduction: no TT hint at deep nodes → search one ply shallower
    val newDepth = if ttMove == Move.None && depth >= Depth(SearchConfig.iirMinDepth) then
      SearchStats.iirReductions += 1
      depth - 1
    else depth

    if !isPvNode && !isRootNode && attemptNullMove(position, newDepth, beta, ply) then return beta

    val inCheck = position.isSideInCheck(position.activeSide)

    val extension   = if inCheck && (!SearchConfig.checkExtPvOnly || isPvNode) then Depth(1) else Depth.Zero
    val searchDepth = newDepth + extension

    if shouldStop() then return PestoEvaluation.evaluate(position)
    if searchDepth.isZero then
      SearchStats.leafNodes += 1
      return quiesce(position, alpha, beta, ply)

    val staticEval =
      if !isPvNode && newDepth <= Depth(SearchConfig.fpMaxDepth) && !inCheck then PestoEvaluation.evaluate(position)
      else Score.Zero

    val canDoFutility =
      !isPvNode && !isRootNode && newDepth <= Depth(SearchConfig.fpMaxDepth) && !inCheck && staticEval + newDepth.toInt * SearchConfig.fpMarginPerDepth <= alpha

    // reverse futility pruning (static null move pruning)
    if !isPvNode && !isRootNode && newDepth <= Depth(SearchConfig.rfpMaxDepth) && !inCheck then
      val mateGuard = Score.Checkmate - Search.MaxPly.toInt
      if beta < mateGuard && beta > -mateGuard && staticEval - SearchConfig.rfpMarginPerDepth * newDepth.toInt >= beta then
        SearchStats.rfpPrunes += 1
        return staticEval

    // razoring: if static eval is well below alpha at a low depth, just go straight to quiesce
    if !isPvNode && !isRootNode && newDepth <= Depth(SearchConfig.razorMaxDepth) && !inCheck then
      val margin = if newDepth == Depth(1) then SearchConfig.razorMarginD1 else SearchConfig.razorMarginD2
      if staticEval + margin < alpha then
        val qScore = quiesce(position, alpha - 1, alpha, ply)
        SearchStats.razorProbes += 1
        if qScore < alpha then
          SearchStats.razorPrunes += 1
          return qScore

    val moveBuf = moveLists(ply.toInt)
    SearchMoveGen.fillMoveList(position, moveBuf)
    val moveArr   = moveBuf.unsafeBuffer
    val moveCount = moveBuf.size
    val scoreArr  = scoreBuffers(ply.toInt)

    val currentKillers = searchHistory.killersAtPly(ply)

    val prevEntry = stack.at(ply - 1)
    val currEntry = stack.at(ply)
    currEntry.quietsTried.clear()
    currEntry.capturesTried.clear()
    currEntry.bestMove = Move.None
    currEntry.isPvNode = isPvNode

    MoveSorter.sortMoves(
      moveArr,
      scoreArr,
      moveCount,
      position,
      searchHistory,
      position.activeSide.ordinal,
      ttMove,
      ply
    )

    var bestScore          = -Score.Infinity
    var bestMove           = Move.None // for tracking TT move
    var currentAlpha       = alpha
    var legalMovesFound    = 0
    var quietMovesSearched = 0
    var ttFlag             = TTEntry.FlagUpper

    var i = 0
    while i < moveCount do
      val move = MoveSorter.pickNext(moveArr, scoreArr, i, moveCount)

      if canDoFutility &&
        !move.isCapture &&
        !move.isPromotion &&
        move != ttMove &&
        legalMovesFound > 0
      then
        SearchStats.futilityPrunes += 1
        i += 1
      else if !isPvNode &&
        searchDepth <= Depth(SearchConfig.lmpMaxDepth) &&
        !inCheck &&
        !move.isCapture &&
        !move.isPromotion &&
        !currentKillers.doesContain(move) &&
        move != ttMove &&
        quietMovesSearched >= SearchConfig.lmpThresholds(searchDepth.toInt)
      then
        SearchStats.lmpPrunes += 1
        i += 1
      else
        val movingSide = position.activeSide.ordinal
        position.applyMove(move)

        if !position.isSideInCheck(position.activeSide.enemy) then
          val movedPieceType = position.pieceAt(move.to).pieceType
          legalMovesFound += 1

          if !move.isCapture && !move.isPromotion then
            quietMovesSearched += 1
            currEntry.addQuiet(move)
          else if move.isCapture then currEntry.addCapture(move)

          val reduction = if shouldReduce(position, move, i, searchDepth, ply) then computeReduction(searchDepth, i) else Depth.Zero
          var score     = Score.Zero

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

          if score >= beta then
            SearchStats.betaCutoffs += 1
            tt.store(position.zobristHash, move, beta, searchDepth, TTEntry.FlagLower, ply)

            if i == 0 then SearchStats.firstMoveCutoffs += 1
            else if searchHistory.killersAtPly(ply).doesContain(move) then SearchStats.killerCutoffs += 1
            else SearchStats.historyCutoffs += 1

            if !move.isCapture && !move.isPromotion then searchHistory.updateAfterQuietCutoff(position, ply, move, newDepth)
            else if move.isCapture && !move.isPromotion then searchHistory.updateAfterCaptureCutoff(position, ply, move, newDepth)

            return beta

          if score > bestScore then
            bestMove = move
            bestScore = score
          if score > currentAlpha then
            currentAlpha = score
            ttFlag = TTEntry.FlagExact
            currEntry.bestMove = move
        else position.unapplyMove(move) // illegal move

        i += 1

    if stopped then return currentAlpha

    if legalMovesFound == 0 then
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
      MoveSorter.scoreCaptures(captureArr, scoreArr, captureCount, position, null)
      SearchStats.qSearchCapturesGenerated += captureCount

      var i = 0
      while i < captureCount && !shouldStop() do
        val move     = MoveSorter.pickNext(captureArr, scoreArr, i, captureCount)
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
