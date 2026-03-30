package com.ffb.zugzwang.core

/**
 * Dynamic time manager for iterative deepening.
 *
 * Consulted once per completed depth (not in the hot search loop). All fields
 * are primitives — no allocation, no Option, no boxing.
 *
 * The core insight: stop searching early when the best move is stable across
 * multiple depths; extend time when the position is unclear (best move keeps
 * changing or score is dropping).
 */
final class TimeManager:
  private var softLimitMs: Long      = 0L
  private var hardLimitMs: Long      = 0L
  private var startTimeMs: Long      = 0L
  private var lastBestMove: Int      = 0
  private var bestMoveStability: Int = 0
  private var lastScore: Int         = 0

  /** Initialize for a new search. Called before iterative deepening begins. */
  def init(softMs: Long, hardMs: Long, startMs: Long): Unit =
    softLimitMs = softMs
    hardLimitMs = hardMs
    startTimeMs = startMs
    lastBestMove = 0
    bestMoveStability = 0
    lastScore = 0

  /**
   * Called after each completed ID iteration. Returns true if the search should
   * continue to the next depth.
   *
   * @param bestMoveValue
   *   Move.value (Int) of the best move at this depth
   * @param score
   *   Score (Int) at this depth
   * @param depth
   *   Depth just completed
   * @param nowMs
   *   Current time in milliseconds
   */
  def shouldContinue(bestMoveValue: Int, score: Int, depth: Int, nowMs: Long): Boolean =
    // No time limit (depth-only mode): always continue
    if softLimitMs >= Long.MaxValue / 4 then return true

    // Track best move stability
    if bestMoveValue == lastBestMove then bestMoveStability += 1
    else bestMoveStability = 0

    // Detect score drops of >30cp vs previous depth.
    // Require depth >= 3: avoids reacting to normal odd/even oscillation between
    // depth 1 (optimistic, no reply considered) and depth 2.
    val scoreDrop = depth >= 3 && score < lastScore - 30

    lastBestMove = bestMoveValue
    lastScore = score

    val elapsed = nowMs - startTimeMs

    // Adjust the effective soft limit based on position clarity.
    // softLimitMs and hardLimitMs are both relative budgets (ms from search start).
    val adjustedSoft =
      if bestMoveStability >= 6 then softLimitMs / 2                    // very stable: stop at 50%
      else if bestMoveStability >= 4 then softLimitMs * 7 / 10          // stable: stop at 70%
      else if scoreDrop then math.min(softLimitMs * 3 / 2, hardLimitMs) // trouble: extend to hard limit
      else softLimitMs

    elapsed < adjustedSoft
