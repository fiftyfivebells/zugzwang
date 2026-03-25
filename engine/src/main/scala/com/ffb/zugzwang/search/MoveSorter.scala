package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.{MutablePosition, Piece, PieceType, Square}
import com.ffb.zugzwang.core.{Killers, Ply, Score, ScoreBuffer}
import com.ffb.zugzwang.evaluation.MoveOrderingTables
import com.ffb.zugzwang.move.{Move, MoveType}

object MoveSorter:

  private val Killer1Bonus    = Score(70000)
  private val Killer2Bonus    = Score(60000)
  private val TTMoveScore     = Score(2000000)
  private val CaptureBase     = Score(100000)
  private val PromotionBase   = Score(80000)
  private val CastleBonus     = Score(500)
  private val DoublePushBonus = Score(40)

  // breaks ties between quiet moves
  private inline val PstDeltaWeight = 2
  private inline val DevelopBonus   = 10

  def scoreMove(move: Move, position: MutablePosition): Score =
    val mover: Piece = position.pieceAt(move.from)

    // prioritize promotions
    if move.isPromotion then
      val promoValue = move.promotion.value
      val capExtra =
        if move.moveType == MoveType.CapturePromotion then Score(10000) else Score.Zero
      return PromotionBase + capExtra + promoValue

    // then captures
    if move.isCapture then
      val victimValue =
        if move.moveType == MoveType.EnPassant then PieceType.Pawn.value
        else
          val captured = position.pieceAt(move.to)
          if captured.isNoPiece then 0 else captured.materialValue

      return CaptureBase + (victimValue * 10) - mover.materialValue

    // castles
    move.moveType match
      case MoveType.CastleKingside | MoveType.CastleQueenside =>
        return CastleBonus
      case _ =>

    // pawn double push
    if move.moveType == MoveType.DoublePush then ()

    // quiet moves
    val beforePst = MoveOrderingTables.value(mover, move.from)
    val afterPst  = MoveOrderingTables.value(mover, move.to)

    val delta    = afterPst - beforePst
    val pstDelta = if mover.isWhite then delta else -delta

    Score(pstDelta * PstDeltaWeight)

  def sortMoves(
    moves: Array[Move],
    scores: ScoreBuffer,
    meta: MoveMetadata,
    count: Int,
    position: MutablePosition,
    searchHistory: SearchHistory,
    side: Int,
    ttMove: Move = Move.None,
    ply: Ply = Ply.Base
  ): Unit =
    val killers = searchHistory.killersAtPly(ply)
    var i       = 0
    while i < count do
      val move = moves(i)
      meta.populate(i, move, position, searchHistory, ply)

      if move == ttMove then scores.setScore(i, TTMoveScore)
      else if meta.isPromotion(i) then
        val promoValue = move.promotion.value
        val capExtra =
          if move.moveType == MoveType.CapturePromotion then Score(10000) else Score.Zero
        scores.setScore(i, PromotionBase + capExtra + promoValue)
      else if meta.isCapture(i) then
        val baseScore = scoreMove(move, position)
        val seeBonus  = if meta.isGoodNoisy(i) then Score(10000) else Score.Zero
        val attacker  = position.pieceAt(move.from)
        val victim    = position.pieceAt(move.to)
        val capScore =
          if !victim.isNoPiece then searchHistory.captureScore(attacker, victim, move.to)
          else Score.Zero
        scores.setScore(i, baseScore + seeBonus + capScore / 16)
      else if move == killers.first then scores.setScore(i, Killer1Bonus)
      else if move == killers.second then scores.setScore(i, Killer2Bonus)
      else
        val positionalScore = scoreMove(move, position)
        scores.setScore(i, positionalScore + meta.historyScore(i))
      i += 1

  inline def pickNext(
    moves: Array[Move],
    scores: ScoreBuffer,
    meta: MoveMetadata | Null,
    index: Int,
    limit: Int
  ): Move =
    var bestIdx   = index
    var bestScore = scores.getScore(index)
    var j         = index + 1
    while j < limit do
      val currentScore = scores.getScore(j)
      if currentScore > bestScore then
        bestScore = currentScore
        bestIdx = j
      j += 1
    if bestIdx != index then
      val tmpM = moves(index); moves(index) = moves(bestIdx); moves(bestIdx) = tmpM
      val tmpS = scores.getScore(index); scores.setScore(index, scores.getScore(bestIdx)); scores.setScore(bestIdx, tmpS)
      if meta != null then meta.swap(index, bestIdx)
    moves(index)

  def scoreCaptures(
    moves: Array[Move],
    scores: ScoreBuffer,
    count: Int,
    position: MutablePosition,
    capHist: Array[Int] = null
  ): Unit =
    var i = 0
    while i < count do
      val move     = moves(i)
      val victim   = position.pieceAt(move.to)
      val attacker = position.pieceAt(move.from)
      val capScore =
        if capHist != null && !victim.isNoPiece then capHist(attacker.index * 384 + move.to.toInt * 6 + victim.pieceType)
        else 0
      scores.setScore(i, Score((victim.materialValue * 10) - attacker.materialValue + capScore / 16))
      i += 1
