package com.ffb.zugzwang.core

final case class TimeWindow(
  softDeadline: SearchTime,
  hardDeadline: SearchTime
)

object TimeControl:
  def computeTimeWindow(softBudgetMs: SearchTime, hardBudgetMs: SearchTime): TimeWindow =
    if softBudgetMs.isMax then TimeWindow(softBudgetMs, hardBudgetMs)
    else
      val now  = SearchTime.currentTime
      val soft = now + softBudgetMs.toLong
      val hard = now + hardBudgetMs.toLong
      TimeWindow(soft, hard)
