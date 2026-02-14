package com.ffb.zugzwang.core

final case class TimeWindow(
  softDeadline: SearchTime,
  hardDeadline: SearchTime
)

object TimeControl:
  private val OverheadMs: Long      = 10L // TODO: maybe make this configurable?
  private val InstantCutoffMs: Long = 5L
  private val SoftRatio: Double     = 0.85

  def computeTimeWindow(budgetMs: SearchTime): TimeWindow =
    if budgetMs.isMax then TimeWindow(budgetMs - OverheadMs, budgetMs)
    else
      val now        = SearchTime.currentTime
      val safeBudget = Math.max(0, budgetMs.toLong - OverheadMs)
      val soft       = now + (safeBudget * SoftRatio).toLong
      val hard       = now + safeBudget

      TimeWindow(soft, hard)

  def shouldSearch(budgetMs: SearchTime): Boolean = budgetMs > InstantCutoffMs
