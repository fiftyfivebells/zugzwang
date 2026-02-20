package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.{MutablePosition, Piece, PieceType, Square}
import com.ffb.zugzwang.core.Score
import com.ffb.zugzwang.evaluation.PieceSquareTables
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
    moves: List[Move],
    position: MutablePosition,
    killers: Array[Move],
    history: Array[Array[Score]],
    ttMove: Move = Move.None
  ): (Array[Move], Array[Score]) =
    val arr    = moves.toArray
    val scores = new Array[Score](arr.length)

    val k1 = if killers.length > 0 then killers(0) else Move.None
    val k2 = if killers.length > 1 then killers(1) else Move.None

    var i = 0
    while i < arr.length do
      val move = arr(i)
      if move == ttMove then scores(i) = TTMoveScore
      else if move == k1 then scores(i) = Killer1Bonus
      else if move == k2 then scores(i) = Killer2Bonus
      else
        val positionalScore = scoreMove(move, position)
        val historyScore    = history(move.from.value)(move.to.value)
        scores(i) = positionalScore + historyScore

      i += 1

    (arr, scores)

  inline def pickNext(moves: Array[Move], scores: Array[Score], index: Int): Move =
    var bestIdx   = index
    var bestScore = scores(index)
    var j         = index + 1
    while j < moves.length do
      if scores(j) > bestScore then
        bestScore = scores(j)
        bestIdx = j
      j += 1
    if bestIdx != index then
      val tmpM = moves(index); moves(index) = moves(bestIdx); moves(bestIdx) = tmpM
      val tmpS = scores(index); scores(index) = scores(bestIdx); scores(bestIdx) = tmpS
    moves(index)

  def scoreCaptures(moves: Array[Move], position: MutablePosition): Array[Score] =
    val scores = new Array[Score](moves.length)
    var i      = 0
    while i < moves.length do
      val move     = moves(i)
      val victim   = position.pieceAt(move.to)
      val attacker = position.pieceAt(move.from)
      scores(i) = Score((victim.materialValue * 10) - attacker.materialValue)
      i += 1
    scores
