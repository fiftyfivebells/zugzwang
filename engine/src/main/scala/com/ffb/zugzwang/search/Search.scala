package com.ffb.zugzwang.search
import com.ffb.zugzwang.chess.MutablePosition
import com.ffb.zugzwang.core.{Depth, Ply, Score, SearchTime}
import com.ffb.zugzwang.move.Move

final case class SearchLimits(
  depth: Depth = Depth(100),
  moveTime: SearchTime = SearchTime.maxTime,
  endTime: SearchTime = SearchTime.maxTime
)

final case class SearchResult(
  move: Move,
  score: Score
)

//   def storeKiller(ply: Ply, move: Move): Unit =
//     if ply >= Search.MaxPly then return

//     if killers.getFirst(ply) == move then return

//     killers.insertMove(ply, move)

//   private val HistoryMax = 16384

//   def updateHistory(side: Int, from: Int, to: Int, delta: Int): Unit =
//     val entry = history(side)(from)(to)
//     history(side)(from)(to) = entry + delta - entry * math.abs(delta) / HistoryMax

//   def historyBonus(depth: Depth): Score =
//     Score(math.min(depth.toInt * depth.toInt, HistoryMax / 4))

//   private val CaptureHistMax = 16384

//   private inline def capIdx(movingPiece: Int, to: Int, capturedType: Int): Int =
//     movingPiece * 384 + to * 6 + capturedType

//   def updateCaptureHistory(movingPiece: Int, to: Int, capturedType: Int, delta: Int): Unit =
//     val idx   = capIdx(movingPiece, to, capturedType)
//     val entry = captureHistory(idx)
//     captureHistory(idx) = entry + delta - entry * math.abs(delta) / CaptureHistMax

//   def contBase(ply: Ply, offset: Int): Int =
//     val idx = ply.toInt - offset
//     if idx >= 0 && stackTo(idx).isDefined then contHistory.baseOffset(stackPiece(idx), stackTo(idx))
//     else -1

//   def trackQuiet(ply: Ply, move: Move): Unit =
//     val qi = quietsTriedCount(ply.toInt)
//     if qi < 256 then
//       quietsTried(ply.toInt)(qi) = move
//       quietsTriedCount(ply.toInt) = qi + 1

//   def trackCapture(ply: Ply, move: Move): Unit =
//     val ci = capturesTriedCount(ply.toInt)
//     if ci < 64 then
//       capturesTried(ply.toInt * 64 + ci) = move
//       capturesTriedCount(ply.toInt) = ci + 1

//   def updateCaptureHistoryAfterCut(ply: Ply, cutMove: Move, capBonus: Score, position: MutablePosition): Unit =
//     updateCaptureHistory(position.pieceAt(cutMove.from).index, cutMove.to.toInt, position.pieceAt(cutMove.to).pieceType, capBonus.toInt)
//     val cCount  = capturesTriedCount(ply.toInt)
//     val plyBase = ply.toInt * 64
//     var ci      = 0
//     while ci < cCount do
//       val c = capturesTried(plyBase + ci)
//       if c != cutMove then
//         val cVictim = position.pieceAt(c.to)
//         if !cVictim.isNoPiece then updateCaptureHistory(position.pieceAt(c.from).index, c.to.toInt, cVictim.pieceType, -capBonus.toInt)
//       ci += 1

//   def updateContHistoryAfterCut(
//     ply: Ply,
//     cutMove: Move,
//     movedPieceType: PieceType,
//     bonus: Score,
//     contBase1: Int,
//     contBase2: Int,
//     movingSide: Int
//   ): Unit =
// //    Update cont history for the cutting move itself
//     if contBase1 >= 0 then contHistory.update(stackPiece(ply.toInt - 1), stackTo(ply.toInt - 1), movedPieceType, cutMove.to, bonus)
//     if contBase2 >= 0 then contHistory.update(stackPiece(ply.toInt - 2), stackTo(ply.toInt - 2), movedPieceType, cutMove.to, bonus)

//     // Apply malus to all previously tried quiet moves
//     val count = quietsTriedCount(ply.toInt)
//     var qi    = 0
//     while qi < count do
//       val q          = quietsTried(ply.toInt)(qi)
//       val qPieceType = quietsTriedPieceType(ply.toInt)(qi)
//       if q != cutMove then
//         updateHistory(movingSide, q.from.toInt, q.to.toInt, -bonus.toInt)
//         if contBase1 >= 0 then contHistory.update(stackPiece(ply.toInt - 1), stackTo(ply.toInt - 1), qPieceType, q.to, -bonus)
//         if contBase2 >= 0 then contHistory.update(stackPiece(ply.toInt - 2), stackTo(ply.toInt - 2), qPieceType, q.to, -bonus)
//       qi += 1

object Search:
  @volatile
  private var stopRequested = false

  val MaxPly           = Ply(128)
  private val searcher = new Searcher

  def clear(): Unit = searcher.clear()

  def requestStop(): Unit =
    stopRequested = true
    searcher.stopped = true

  def search(position: MutablePosition, limits: SearchLimits): Move =
    stopRequested = false
    searcher.search(position, limits)
