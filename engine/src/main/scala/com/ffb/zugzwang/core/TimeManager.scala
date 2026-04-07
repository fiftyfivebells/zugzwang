package com.ffb.zugzwang.core

object TimeManager:
  // tunable TM parameters (settable via UCI setoption)
  // TODO: these should be made into typed fields
  var tmStabilityCenter: Double  = 4.0
  var tmInstabilityCoeff: Double = 2.14
  var tmInstabilityBase: Double  = 1.02
  var tmHardLimitFraction: Int   = 60 // percentage (0-100)

final class TimeManager:
  // limits get set once per search via the init function
  private var softLimitMs: Long = 0L
  private var hardLimitMs: Long = 0L
  private var startTimeMs: Long = 0L

  // iteration tracking
  private var lastBestMove: Int            = 0
  private var lastBestMoveDepth: Int       = 1
  private var totalBestMoveChanges: Double = 0.0

  // score tracking (buffer of last 4 iteration scores)
  private val iterScores: Array[Int] = new Array[Int](4)
  private var iterIdx: Int           = 0

  // carry over between moves (persists across searches)
  private var previousTimeReduction: Double = 1.0
  private var bestPreviousAvgScore: Int     = 0

  // diagnostic fields
  private var lastFallingEval: Double  = 1.0
  private var lastReduction: Double    = 1.0
  private var lastInstability: Double  = 1.02
  private var lastNodeTM: Double       = 1.0
  private var lastAdjustedSoft: Double = 0.0
  private var lastElapsed: Long        = 0L

  def init(softMs: Long, hardMs: Long, startMs: Long): Unit =
    softLimitMs = softMs
    hardLimitMs = hardMs
    startTimeMs = startMs
    lastBestMove = 0
    lastBestMoveDepth = 1
    totalBestMoveChanges = 0.0
    java.util.Arrays.fill(iterScores, 0)
    iterIdx = 0

  def shouldContinue(
    bestMoveValue: Int,
    score: Int,
    depth: Int,
    nowMs: Long,
    bestMoveNodeFraction: Double
  ): Boolean =
    if softLimitMs >= Long.MaxValue / 4 then return true

    // best move change tracking
    // halve each iteration so recent changes weigh more
    totalBestMoveChanges /= 2.0
    if bestMoveValue != lastBestMove then
      totalBestMoveChanges += 1.0
      lastBestMoveDepth = depth
    lastBestMove = bestMoveValue

    val prevIterScore = iterScores(iterIdx)
    iterScores(iterIdx) = score
    iterIdx = (iterIdx + 1) & 3

    // TODO: tune the values below. I took them from a mix of Calvin and Stockfish
    // everything with a comment is tunable and probably needs some trial and error
    // to figure out the best values for this engine

    // falling eval (0.580 to 1.667)
    // responds to score drops vs both the previous search's average and the
    // previous iteration within this search
    val fallingEval = math.max(
      0.580,
      math.min(
        1.667,
        (11.85 + 2.24 * (bestPreviousAvgScore - score) +
          0.93 * (prevIterScore - score)) / 100.0
      )
    )

    // best move stability (sigmoid, ~0.50 to ~2.0)
    // smooth logistic curve centered at (lastBestMoveDepth + 12.15).
    // when many iterations pass without the best move changing,
    // timeReduction drops below 1.0, which increases reduction
    val k             = 0.51
    val center        = lastBestMoveDepth + TimeManager.tmStabilityCenter
    val timeReduction = 0.66 + 0.85 / (0.98 + math.exp(-k * (depth - center)))
    val reduction     = (1.43 + previousTimeReduction) / (2.28 * timeReduction)
    previousTimeReduction = timeReduction

    // best move instability (>= 1.02)
    // more best-move changes across iterations means more time.
    val bestMoveInstability = TimeManager.tmInstabilityBase + TimeManager.tmInstabilityCoeff * totalBestMoveChanges

    // node tm (0.76 or 1.0)
    // If ≥93.3% of nodes went to the best move, the position is trivially decided.
    val nodeTmFactor =
      if bestMoveNodeFraction >= 0.9334 then 0.76
      else 1.0

    val adjustedSoft = softLimitMs.toDouble *
      fallingEval * reduction * bestMoveInstability * nodeTmFactor

    val elapsed        = nowMs - startTimeMs
    val effectiveLimit = math.min(adjustedSoft, hardLimitMs.toDouble)

    lastFallingEval = fallingEval
    lastReduction = reduction
    lastInstability = bestMoveInstability
    lastNodeTM = nodeTmFactor
    lastAdjustedSoft = adjustedSoft
    lastElapsed = elapsed

    elapsed < effectiveLimit

  def onSearchComplete(bestScore: Int): Unit = bestPreviousAvgScore = bestScore

  def diagnosticString: String =
    f"soft=$softLimitMs hard=$hardLimitMs elapsed=$lastElapsed " +
      f"adjSoft=${lastAdjustedSoft.toLong} " +
      f"falling=$lastFallingEval%.3f reduction=$lastReduction%.3f " +
      f"instability=$lastInstability%.3f nodeTM=$lastNodeTM%.2f"

  def clearCarryOver(): Unit =
    previousTimeReduction = 1.0
    bestPreviousAvgScore = 0
