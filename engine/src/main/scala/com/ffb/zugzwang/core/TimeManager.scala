package com.ffb.zugzwang.core

/**
 * Dynamic time manager using the Stockfish/Calvin model.
 *
 * Four continuous multiplicative factors adjust the soft limit after each
 * completed iterative deepening iteration:
 *
 * adjustedSoft = soft × fallingEval × reduction × bestMoveInstability ×
 * nodeTmFactor
 *
 * All fields are primitives — no allocation, no Option, no boxing.
 */
final class TimeManager:
  // Limits (set once per search via init)
  private var softLimitMs: Long = 0L
  private var hardLimitMs: Long = 0L
  private var startTimeMs: Long = 0L

  // Iteration tracking
  private var lastBestMove: Int            = 0
  private var lastBestMoveDepth: Int       = 1
  private var totalBestMoveChanges: Double = 0.0

  // Score tracking (ring buffer of last 4 iteration scores)
  private val iterScores: Array[Int] = new Array[Int](4)
  private var iterIdx: Int           = 0

  // Carry-over between moves (persists across searches)
  private var previousTimeReduction: Double = 1.0
  private var bestPreviousAvgScore: Int     = 0

  /** Initialize for a new search. Called before iterative deepening begins. */
  def init(softMs: Long, hardMs: Long, startMs: Long): Unit =
    softLimitMs = softMs
    hardLimitMs = hardMs
    startTimeMs = startMs
    lastBestMove = 0
    lastBestMoveDepth = 1
    totalBestMoveChanges = 0.0
    java.util.Arrays.fill(iterScores, 0)
    iterIdx = 0

  /**
   * Called after each completed iteration. Returns true if search should
   * continue to the next depth.
   *
   * @param bestMoveValue
   *   Move.value (Int) of the best move at this depth
   * @param score
   *   Score (Int, centipawns) at this depth
   * @param depth
   *   Depth just completed (1-indexed)
   * @param nowMs
   *   Current time in milliseconds
   * @param bestMoveNodeFraction
   *   Fraction of total root nodes spent on best move (0.0 to 1.0). Pass 0.0 if
   *   not tracked yet (disables node TM).
   */
  def shouldContinue(
    bestMoveValue: Int,
    score: Int,
    depth: Int,
    nowMs: Long,
    bestMoveNodeFraction: Double
  ): Boolean =
    // Depth-only or infinite mode: always continue
    if softLimitMs >= Long.MaxValue / 4 then return true

    // --- Track best move changes ---
    // Age out old changes (halve each iteration, so recent changes weigh more)
    totalBestMoveChanges /= 2.0
    if bestMoveValue != lastBestMove then
      totalBestMoveChanges += 1.0
      lastBestMoveDepth = depth
    lastBestMove = bestMoveValue

    // --- Update score ring buffer ---
    val prevIterScore = iterScores(iterIdx)
    iterScores(iterIdx) = score
    iterIdx = (iterIdx + 1) & 3

    // --- Factor 1: Falling eval (0.580 to 1.667) ---
    // Responds to score drops vs both the previous search's average and the
    // previous iteration within this search. Proportional, not binary.
    val fallingEval = math.max(
      0.580,
      math.min(
        1.667,
        (11.85 + 2.24 * (bestPreviousAvgScore - score) +
          0.93 * (prevIterScore - score)) / 100.0
      )
    )

    // --- Factor 2: Best move stability (sigmoid, ~0.50 to ~2.0) ---
    // Smooth logistic curve centered at (lastBestMoveDepth + 12.15).
    // When many iterations pass without the best move changing,
    // timeReduction drops below 1.0, which increases `reduction`.
    val k             = 0.51
    val center        = lastBestMoveDepth + 12.15
    val timeReduction = 0.66 + 0.85 / (0.98 + math.exp(-k * (depth - center)))
    val reduction     = (1.43 + previousTimeReduction) / (2.28 * timeReduction)
    previousTimeReduction = timeReduction

    // --- Factor 3: Best move instability (>= 1.02) ---
    // More best-move changes across iterations → more time.
    val bestMoveInstability = 1.02 + 2.14 * totalBestMoveChanges

    // --- Factor 4: Node TM (0.76 or 1.0) ---
    // If ≥93.3% of nodes went to the best move, the position is trivially decided.
    val nodeTmFactor =
      if bestMoveNodeFraction >= 0.9334 then 0.76
      else 1.0

    // --- Combined adjusted soft limit ---
    val adjustedSoft = softLimitMs.toDouble *
      fallingEval * reduction * bestMoveInstability * nodeTmFactor

    val elapsed        = nowMs - startTimeMs
    val effectiveLimit = math.min(adjustedSoft, hardLimitMs.toDouble)

    elapsed < effectiveLimit

  /**
   * Called at the end of each search (after bestmove is printed) to carry score
   * context to the next move's time management.
   *
   * @param bestScore
   *   The final score of the completed search
   */
  def onSearchComplete(bestScore: Int): Unit =
    bestPreviousAvgScore = bestScore

  /** Reset carry-over state. Called on ucinewgame. */
  def clearCarryOver(): Unit =
    previousTimeReduction = 1.0
    bestPreviousAvgScore = 0
