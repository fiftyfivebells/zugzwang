package com.ffb.zugzwang.search
import com.ffb.zugzwang.chess.MutablePosition
import com.ffb.zugzwang.core.{Depth, KillersList, Node, Ply, Score, SearchTime, TimeControl}
import com.ffb.zugzwang.evaluation.{PestoEvaluation, SEE}
import com.ffb.zugzwang.move.{Move, MoveList}
import com.ffb.zugzwang.tools.DebugLogger

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
  val killers: KillersList = KillersList.initialize(Search.MaxPly.asInt),
  val history: Array[Array[Score]] = Array.ofDim[Score](64, 64),
  val moveLists: Array[MoveList] = Array.fill(Search.MaxPly.asInt + Search.MaxQDepth + 1)(MoveList(256)),
  val scoreBuffers: Array[Array[Score]] = Array.fill(Search.MaxPly.asInt + Search.MaxQDepth + 1)(new Array[Score](256))
):

  def storeKiller(ply: Ply, move: Move): Unit =
    if ply >= Search.MaxPly then return

    if killers.getFirst(ply) == move then return

    killers.insertMove(ply, move)

  def updateHistory(move: Move, depth: Depth): Unit =
    val bonus   = depth.value * depth.value
    val current = history(move.from.value)(move.to.value)
    if current < 50000 then                                                // TODO: magic number alert, make this a constant
      history(move.from.value)(move.to.value) = current * 95 / 100 + bonus // 5% decay

object Search:
  @volatile
  private var stopRequested = false

  val MaxPly     = Ply(128)
  private val tt = new TranspositionTable(256)

  def clear(): Unit = tt.clear()

  def requestStop(): Unit = stopRequested = true

  def search(position: MutablePosition, limits: SearchLimits): Move =
    stopRequested = false

    val now    = SearchTime.currentTime
    val window = TimeControl.computeTimeWindow(limits.moveTime)

    val rootMl = MoveList(256)
    SearchMoveGen.fillMoveList(position, rootMl)
    val legalMoves = rootMl.toArray.filter { move =>
      position.applyMove(move)
      val legal = !position.isSideInCheck(position.activeSide.enemy)
      position.unapplyMove(move)
      legal
    }
    if legalMoves.isEmpty then return Move.None // no legal moves

    val ctx = SearchContext(
      startTime = now,
      endTime = window.hardDeadline,
      depthLimit = limits.depth,
      table = tt
    )
    tt.incrementGeneration()

    val defaultScores = new Array[Score](legalMoves.length)
    MoveSorter.sortMoves(legalMoves, defaultScores, legalMoves.length, position, ctx.killers.basePly, ctx.history)
    val defaultMove = MoveSorter.pickNext(legalMoves, defaultScores, 0, legalMoves.length)

    @tailrec
    def iterativeDeepening(currentDepth: Depth, bestMove: Move, prevScore: Score): Move =
      val now           = SearchTime.currentTime
      val outOfTime     = now >= window.softDeadline
      val depthExceeded = currentDepth > limits.depth

      if outOfTime || depthExceeded || stopRequested then return bestMove

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

    try
      if TimeControl.shouldSearch(limits.moveTime) then iterativeDeepening(Depth(1), defaultMove, Score.Draw)
      else defaultMove
    catch
      case e =>
        DebugLogger.log("CRASH")
        DebugLogger.log(e.getMessage())
        defaultMove

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

    var delta       = 50
    val maxAttempts = 3
    var attempts    = 0

    while (result.score <= alpha || result.score >= beta) && attempts < maxAttempts do
      attempts += 1
      delta = delta * 2

      if result.score <= alpha then
        SearchStats.aspirationFailLows += 1
        alpha = alpha - delta
        if alpha < -Score.Infinity then alpha = -Score.Infinity

      if result.score >= beta then
        SearchStats.aspirationFailHighs += 1
        beta = beta + delta
        if beta > Score.Infinity then beta = Score.Infinity

      result = findBestMove(position, depth, alpha, beta, ctx)

    result

  def findBestMove(
    position: MutablePosition,
    depth: Depth,
    alpha: Score,
    beta: Score,
    ctx: SearchContext
  ): SearchResult =
    val ttEntry = ctx.table.probe(position.zobristHash)
    val ttMove  = if ttEntry.isDefined then ttEntry.move else Move.None

    val moveBuf = ctx.moveLists(0)
    SearchMoveGen.fillMoveList(position, moveBuf)
    val moveArr   = moveBuf.unsafeBuffer
    val moveCount = moveBuf.size
    val scoreArr  = ctx.scoreBuffers(0)
    MoveSorter.sortMoves(moveArr, scoreArr, moveCount, position, ctx.killers.basePly, ctx.history, ttMove)

    @tailrec
    def loop(
      moveIndex: Int,
      bestMove: Move,
      alpha: Score,
      beta: Score,
      legalMoves: Int,
      firstLegalMove: Move = Move.None,
      ply: Ply = Ply.Base
    ): SearchResult =
      if moveIndex >= moveCount then
        if legalMoves == 0 then
          if position.isSideInCheck(position.activeSide) then SearchResult(Move.None, -Score.Checkmate)
          else SearchResult(Move.None, Score.Stalemate)
        else
          val finalMove = if bestMove == Move.None then firstLegalMove else bestMove
          val ttFlag    = if bestMove == Move.None then TTEntry.FlagUpper else TTEntry.FlagExact
          ctx.table.store(position.zobristHash, finalMove, alpha, depth, ttFlag, ply)
          SearchResult(finalMove, alpha)
      else
        val move = MoveSorter.pickNext(moveArr, scoreArr, moveIndex, moveCount)
        position.applyMove(move)
        if position.isSideInCheck(position.activeSide.enemy) then
          position.unapplyMove(move)
          loop(moveIndex + 1, bestMove, alpha, beta, legalMoves, firstLegalMove, ply)
        else
          val newFirstLegalMove = if firstLegalMove == Move.None then move else firstLegalMove

          val rawScore =
            if legalMoves == 0 then
              // first move, full window search
              -negamax(position, depth - 1, -beta, -alpha, ctx, ply + 1)
            else
              val nullScore = -negamax(position, depth - 1, -alpha - 1, -alpha, ctx, ply + 1)
              // need to re-search with full window if nullScore is in [alpha..beta]
              if nullScore > alpha && nullScore < beta then -negamax(position, depth - 1, -beta, -alpha, ctx, ply + 1)
              else nullScore

          val comparisonScore =
            if rawScore == Score.Draw then
              alpha match
                case a if a < Score.Draw => rawScore + Score.DrawBias
                case a if a > Score.Draw => rawScore - Score.DrawBias
                case _                   => rawScore
            else rawScore

          val (newAlpha, newBestMove) = if comparisonScore > alpha then (rawScore, move) else (alpha, bestMove)
          position.unapplyMove(move)

          loop(moveIndex + 1, newBestMove, newAlpha, beta, legalMoves + 1, newFirstLegalMove, ply)

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
      ply.asInt == 0 ||
      beta >= Score.Infinity ||
      !position.hasMajorPieces(position.activeSide)
    then false
    else
      try
        position.applyNullMove
        val score = -negamax(position, depth - 1 - nullMoveReduction(depth), -beta, -beta + 1, ctx, ply + 1)
        score >= beta
      finally position.unapplyNullMove

  private inline def computeReduction(depth: Depth, moveIndex: Int): Depth =
    if depth < Depth(3) || moveIndex < 3 then Depth.Zero
    else
      val base = math.log(depth.value) * math.log(moveIndex) / 2.5
      Depth(math.max(1, math.min(base.floor.toInt, depth.value - 1)))

  private inline def shouldReduce(
    position: MutablePosition,
    move: Move,
    moveIndex: Int,
    depth: Depth,
    ply: Ply,
    ctx: SearchContext
  ): Boolean =
    val inCheck        = position.isSideInCheck(position.activeSide.enemy)
    val givesCheck     = position.isSideInCheck(position.activeSide)
    val isCapture      = move.isCapture
    val isPromotion    = move.isPromotion
    val currentKillers = ctx.killers.atPly(ply)
    val isKiller       = currentKillers.doesContain(move)
    val highHistory    = ctx.history(move.from.value)(move.to.value) > 1000
    val isGoodCapture  = isCapture && SEE.seeGE(position, move)

    depth >= Depth(3) &&
    moveIndex >= 3 &&
    !inCheck &&
    !givesCheck &&
    !isCapture &&
    !isPromotion &&
    !isKiller &&
    !highHistory &&
    !isGoodCapture

  // TODO: look into maybe making this a tail recursive function
  private def negamax(position: MutablePosition, depth: Depth, alpha: Score, beta: Score, ctx: SearchContext, ply: Ply): Score =
    SearchStats.nodes += 1
    ctx.nodes = ctx.nodes + 1

    if ply >= MaxPly then return PestoEvaluation.evaluate(position)

    if position.halfMoveClock >= 100 then return Score.Draw
    if ply > 0 && position.isRepetition then return Score.Draw

    SearchStats.ttProbes += 1
    val ttEntry = ctx.table.probe(position.zobristHash)
    var ttMove  = Move.None

    if ttEntry.isDefined then
      SearchStats.ttHits += 1
      ttMove = ttEntry.move
      if ply.asInt > 0 && ttEntry.canCutoff(depth, alpha, beta, ply) then return ttEntry.score(ply)

    // internal iterative reduction: no TT hint at deep nodes → search one ply shallower
    val newDepth = if ttMove == Move.None && depth >= Depth(4) then
      SearchStats.iirReductions += 1
      depth - 1
    else depth

    if attemptNullMove(position, newDepth, beta, ctx, ply) then return beta

    if shouldStop(ctx) then return PestoEvaluation.evaluate(position)
    if newDepth.isZero then
      SearchStats.leafNodes += 1
      return quiesce(position, alpha, beta, ctx, ply)

    val inCheck = position.isSideInCheck(position.activeSide)

    val (canDoFutility, futilityMargin, staticEval) =
      if newDepth <= Depth(3) && !inCheck then
        val eval   = PestoEvaluation.evaluate(position)
        val margin = newDepth.value * 150
        (eval + margin <= alpha, margin, eval)
      else (false, 0, Score.Zero)

    // reverse futility pruning (static null move pruning)
    if newDepth <= Depth(3) && !inCheck then
      val mateGuard = Score.Checkmate - MaxPly.asInt
      if beta < mateGuard && beta > -mateGuard && staticEval - 80 * newDepth.value >= beta then
        SearchStats.rfpPrunes += 1
        return staticEval

    val moveBuf = ctx.moveLists(ply.asInt)
    SearchMoveGen.fillMoveList(position, moveBuf)
    val moveArr   = moveBuf.unsafeBuffer
    val moveCount = moveBuf.size
    val scoreArr  = ctx.scoreBuffers(ply.asInt)

    val currentKillers = ctx.killers.atPly(ply)
    MoveSorter.sortMoves(moveArr, scoreArr, moveCount, position, currentKillers, ctx.history, ttMove)

    var bestScore       = -Score.Infinity
    var bestMove        = Move.None // for tracking TT move
    var currentAlpha    = alpha
    var legalMovesFound = 0
    var ttFlag          = TTEntry.FlagUpper

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
      else
        position.applyMove(move)

        if !position.isSideInCheck(position.activeSide.enemy) then
          legalMovesFound += 1

          val reduction    = if shouldReduce(position, move, i, newDepth, ply, ctx) then computeReduction(newDepth, i) else Depth.Zero
          var score        = Score.Zero
          val isFullWindow = beta - alpha > 1

          if reduction > Depth.Zero then
            // reduced depth, use null window
            SearchStats.lmrReductions += 1
            score = -negamax(position, newDepth - 1 - reduction, -currentAlpha - 1, -currentAlpha, ctx, ply + 1)

            // full depth, still use null window
            if score > currentAlpha then
              SearchStats.lmrResearches += 1
              score = -negamax(position, newDepth - 1, -currentAlpha - 1, -currentAlpha, ctx, ply + 1)
          // no LMR, not first move -> PVS null window probe
          else if legalMovesFound > 1 then score = -negamax(position, newDepth - 1, -currentAlpha - 1, -currentAlpha, ctx, ply + 1)
          // first legal move, always search with full window
          else score = -negamax(position, newDepth - 1, -beta, -currentAlpha, ctx, ply + 1)

          // full window re-search -> only at PV nodes and only if null window beat alpha
          if score > currentAlpha && isFullWindow && legalMovesFound > 1 then
            SearchStats.pvsReSearches += 1
            score = -negamax(position, newDepth - 1, -beta, -currentAlpha, ctx, ply + 1)

          position.unapplyMove(move)

          if score >= beta then
            SearchStats.betaCutoffs += 1
            ctx.table.store(position.zobristHash, move, beta, newDepth, TTEntry.FlagLower, ply)

            if i == 0 then SearchStats.firstMoveCutoffs += 1
            else if currentKillers.doesContain(move) then SearchStats.killerCutoffs += 1
            else SearchStats.historyCutoffs += 1

            if !move.isCapture then
              ctx.storeKiller(ply, move)
              ctx.updateHistory(move, newDepth)

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
      if position.isSideInCheck(position.activeSide) then -Score.Checkmate + ply.asInt
      else Score.Stalemate
    else
      ctx.table.store(position.zobristHash, bestMove, bestScore, newDepth, ttFlag, ply)
      bestScore

  private val QFutilityMargin = 150
  val MaxQDepth               = 10

  private def quiesce(position: MutablePosition, alpha: Score, beta: Score, ctx: SearchContext, ply: Ply, qDepth: Int = 0): Score =
    SearchStats.qNodes += 1
    ctx.nodes = ctx.nodes + 1
    SearchStats.qSearchMaxDepth = Math.max(qDepth, SearchStats.qSearchMaxDepth)

    if qDepth >= MaxQDepth then return PestoEvaluation.evaluate(position)

    SearchStats.qTtProbes += 1
    val ttEntry = ctx.table.probe(position.zobristHash)
    if ttEntry.isDefined then
      SearchStats.qTtHits += 1
      if ttEntry.canCutoff(Depth.Zero, alpha, beta, ply) then return ttEntry.score(ply)

    if shouldStop(ctx) then return PestoEvaluation.evaluate(position)

    if position.isSideInCheck(position.activeSide) then
      // Stand-pat is invalid when in check — must search all evasions
      val moveBuf = ctx.moveLists(ply.asInt)
      SearchMoveGen.fillMoveList(position, moveBuf)
      val moveArr      = moveBuf.unsafeBuffer
      val moveCount    = moveBuf.size
      var bestScore    = -Score.Infinity
      var currentAlpha = alpha
      var legalMoves   = 0

      var i = 0
      while i < moveCount && !shouldStop(ctx) do
        val move = moveArr(i)
        position.applyMove(move)
        if !position.isSideInCheck(position.activeSide.enemy) then
          legalMoves += 1
          val score = -quiesce(position, -beta, -currentAlpha, ctx, ply + 1, qDepth + 1)
          position.unapplyMove(move)
          if score >= beta then return beta
          if score > bestScore then bestScore = score
          if score > currentAlpha then currentAlpha = score
        else position.unapplyMove(move)
        i += 1

      if ctx.stopped then return bestScore
      if legalMoves == 0 then return -Score.Checkmate + ply.asInt
      bestScore
    else
      val standPat = PestoEvaluation.evaluate(position)
      if standPat >= beta then return beta

      var currentAlpha = Score.max(alpha, standPat)

      val captureBuf = ctx.moveLists(ply.asInt)
      SearchMoveGen.fillCaptures(position, captureBuf)
      val captureArr   = captureBuf.unsafeBuffer
      val captureCount = captureBuf.size
      val scoreArr     = ctx.scoreBuffers(ply.asInt)
      MoveSorter.scoreCaptures(captureArr, scoreArr, captureCount, position)
      SearchStats.qSearchCapturesGenerated += captureCount

      var i = 0
      while i < captureCount && !shouldStop(ctx) do
        val move     = MoveSorter.pickNext(captureArr, scoreArr, i, captureCount)
        val captured = position.pieceAt(move.to)

        if standPat + captured.materialValue + QFutilityMargin >= currentAlpha then
          if SEE.seeGE(position, move) then
            position.applyMove(move)
            if !position.isSideInCheck(position.activeSide.enemy) then
              val score = -quiesce(position, -beta, -currentAlpha, ctx, ply + 1, qDepth + 1)
              position.unapplyMove(move)
              if score >= beta then return beta
              if score > currentAlpha then currentAlpha = score
            else position.unapplyMove(move)
          else SearchStats.seePrunesQSearch += 1

        SearchStats.qSearchMovesSearched += 1
        i += 1

      currentAlpha

  private inline def shouldStop(ctx: SearchContext): Boolean =
    if ctx.stopped then true
    else if (ctx.nodes & 2047L).isZero && SearchTime.currentTime >= ctx.endTime then
      ctx.stopped = true
      true
    else false
