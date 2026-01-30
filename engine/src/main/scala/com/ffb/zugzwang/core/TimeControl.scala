package com.ffb.zugzwang.core

final case class TimeWindow(
  softDeadline: SearchTime,
  hardDeadline: SearchTime
)

object TimeControl:
  private val OverheadMs: Long      = 10L // TODO: maybe make this configurable?
  private val InstantCutoffMs: Long = 5L

  def computeTimeWindow(budgetMs: SearchTime): TimeWindow =
    if budgetMs.isMax then TimeWindow(budgetMs - OverheadMs, budgetMs)
    else
      val now  = SearchTime.currentTime
      val soft = now + Math.max(0, budgetMs.toLong - OverheadMs)
      val hard = now + budgetMs

      TimeWindow(soft, hard)

  def shouldSearch(budgetMs: SearchTime): Boolean = budgetMs > InstantCutoffMs
