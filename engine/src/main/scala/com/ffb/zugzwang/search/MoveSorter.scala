package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.{MutablePosition, Piece, PieceType, Square}
import com.ffb.zugzwang.core.{Killers, Score, ScoreBuffer}
import com.ffb.zugzwang.evaluation.{PieceSquareTables, SEE}
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
    val beforePst = PieceSquareTables.value(PieceSquareTables.MidgamePieceSquareTables, mover, move.from)
    val afterPst  = PieceSquareTables.value(PieceSquareTables.MidgamePieceSquareTables, mover, move.to)

    val delta    = afterPst - beforePst
    val pstDelta = if mover.isWhite then delta else -delta

    Score(pstDelta * PstDeltaWeight)

  def sortMoves(
    moves: Array[Move],
    scores: ScoreBuffer,
    count: Int,
    position: MutablePosition,
    killers: Killers,
    history: Array[Array[Array[Int]]],
    sideToMove: Int,
    ttMove: Move = Move.None,
    contHistoryArr: Array[Score] = Array[Score](),
    contBase1: Int = -1,
    contBase2: Int = -1,
    captureHistory: Array[Int] = null
  ): Unit =
    var i = 0
    while i < count do
      val move = moves(i)
      if move == ttMove then scores.setScore(i, TTMoveScore)
      else if move.isCapture then
        val baseScore = scoreMove(move, position)
        val seeBonus  = if SEE.seeGE(position, move) then Score(10000) else Score.Zero
        val capScore =
          if captureHistory != null then
            val attacker = position.pieceAt(move.from)
            val victim   = position.pieceAt(move.to)
            if !victim.isNoPiece then captureHistory(attacker.index * 384 + move.to.toInt * 6 + victim.pieceType)
            else 0
          else 0
        scores.setScore(i, baseScore + seeBonus + Score(capScore / 16))
      else if move == killers.first then scores.setScore(i, Killer1Bonus)
      else if move == killers.second then scores.setScore(i, Killer2Bonus)
      else
        val positionalScore = scoreMove(move, position)
        val historyScore    = Score(history(sideToMove)(move.from.toInt)(move.to.toInt))
        val ch1ch2 =
          if contBase1 > -1 || contBase2 > -1 then
            val moverType = position.pieceAt(move.from).pieceType
            val innerOff  = moverType * 64 + move.to.toInt

            val ch1 = if contBase1 > -1 then contHistoryArr(contBase1 + innerOff) else Score.Zero
            val ch2 = if contBase2 > -1 then contHistoryArr(contBase2 + innerOff) else Score.Zero

            (ch1 + ch2) / 2
          else Score.Zero

        scores.setScore(i, positionalScore + historyScore + ch1ch2)

      i += 1

  inline def pickNext(moves: Array[Move], scores: ScoreBuffer, index: Int, limit: Int): Move =
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
