package com.ffb.zugzwang.search
import com.ffb.zugzwang.chess.MutablePosition
import com.ffb.zugzwang.core.{Depth, Ply, Score, SearchTime}
import com.ffb.zugzwang.move.Move

final case class SearchLimits(
  depth: Depth = Depth(100),
  moveTime: SearchTime = SearchTime.maxTime,
  endTime: SearchTime = SearchTime.maxTime,
  ply: Int = 0
)

final case class SearchResult(
  move: Move,
  score: Score
)

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
