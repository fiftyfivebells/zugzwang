package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.{MutablePosition, Piece, PieceType, Square}
import com.ffb.zugzwang.core.{Depth, Killers, KillersList, Ply, Score}
import com.ffb.zugzwang.move.Move

final class SearchHistory(private val stack: SearchStack):
  private val killersList    = KillersList.initialize(Search.MaxPly.toInt)
  private val quietHistory   = QuietHistory.empty
  private val captureHistory = CaptureHistory.empty
  private val contHistory    = ContinuationHistory.empty

  private val QuietHistoryBonusScale = Score(1202)
  private val QuietHistoryBonusMax   = Score(207)
  private val QuietHistoryMalusScale = Score(200)
  private val QuietHistoryMalusMax   = Score(1255)

  private val ContHistoryBonusScale = Score(204)
  private val ContHistoryBonusMax   = Score(1229)
  private val ContHistoryMalusScale = Score(205)
  private val ContHistoryMalusMax   = Score(1067)

  private val CaptureHistoryBonusScale = Score(192)
  private val CaptureHistoryBonusMax   = Score(1250)
  private val CaptureHistoryMalusScale = Score(211)
  private val CaptureHistoryMalusMax   = Score(1185)

  private def historyBonus(depth: Depth): Score =
    Score(math.min(depth.toInt * depth.toInt, SearchConfig.historyMax / 4))

  private def historyMalus(depth: Depth): Score =
    -historyBonus(depth)

  private def storeKiller(ply: Ply, move: Move): Unit =
    if ply < Ply(128) && killersList.getFirst(ply) != move then killersList.insertMove(ply, move)

  private def contHistScore(
    ply: Ply,
    offset: Int,
    piece: Piece,
    to: Square
  ): Score =
    val prev = stack.at(ply - offset)
    if !prev.move.isNoMove && prev.piece.pieceType.isDefined then contHistory.get(prev.piece, piece, prev.move.to, to)
    else Score.Zero

  private def updateContHist(
    ply: Ply,
    piece: Piece,
    to: Square,
    delta: Score
  ): Unit =
    val prev1 = stack.at(ply - 1)
    if !prev1.move.isNoMove && prev1.piece.pieceType.isDefined then contHistory.add(prev1.piece, piece, prev1.move.to, to, delta)

    val prev2 = stack.at(ply - 2)
    if !prev2.move.isNoMove && prev2.piece.pieceType.isDefined then contHistory.add(prev2.piece, piece, prev2.move.to, to, delta)

  def quietMoveScore(
    ply: Ply,
    move: Move,
    piece: Piece
  ): Score =
    val qh  = quietHistory.get(piece, move.to)
    val ch1 = contHistScore(ply, 1, piece, move.to)
    val ch2 = contHistScore(ply, 2, piece, move.to)

    qh + ch1 + ch2

  def quietScore(piece: Piece, to: Square): Score =
    quietHistory.get(piece, to)

  def captureScore(moving: Piece, captured: Piece, to: Square): Score =
    captureHistory.get(moving, captured, to)

  def killersAtPly(ply: Ply): Killers = killersList.atPly(ply)

  def updateAfterQuietCutoff(
    position: MutablePosition,
    ply: Ply,
    cutoffMove: Move,
    depth: Depth
  ): Unit =
    val bonus       = historyBonus(depth)
    val malus       = historyMalus(depth)
    val cutoffPiece = position.pieceAt(cutoffMove.from)

    storeKiller(ply, cutoffMove)
    quietHistory.add(cutoffPiece, cutoffMove.to, bonus)
    updateContHist(ply, cutoffPiece, cutoffMove.to, bonus)

    val entry      = stack.at(ply)
    var i          = 0
    val triedCount = entry.quietsTried.count
    while i < triedCount do
      val q      = entry.quietsTried.unsafeGet(i)
      val qPiece = position.pieceAt(q.from)
      quietHistory.add(qPiece, q.to, malus)
      updateContHist(ply, qPiece, q.to, malus)

      i += 1

  def updateAfterCaptureCutoff(
    position: MutablePosition,
    ply: Ply,
    cutoffMove: Move,
    depth: Depth
  ): Unit =
    val bonus = historyBonus(depth)
    val malus = historyMalus(depth)

    val moving   = position.pieceAt(cutoffMove.from)
    val captured = position.pieceAt(cutoffMove.to)
    if !captured.isNoPiece then captureHistory.add(moving, captured, cutoffMove.to, bonus)

    val entry      = stack.at(ply)
    var i          = 0
    val triedCount = entry.capturesTried.count
    while i < triedCount do
      val c       = entry.capturesTried.unsafeGet(i)
      val cVictim = position.pieceAt(c.to)
      if !cVictim.isNoPiece then
        val cMoving = position.pieceAt(c.from)
        captureHistory.add(cMoving, cVictim, c.to, malus)

      i += 1

  def clear(): Unit =
    killersList.clear()
    quietHistory.clear()
    captureHistory.clear()
    contHistory.clear()
