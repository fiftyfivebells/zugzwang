package com.ffb.zugzwang.search

object SearchStats:
  // general counters
  var nodes: Long     = 0
  var qNodes: Long    = 0
  var leafNodes: Long = 0

  // transposition table (split regular search and quiescence search)
  var ttProbes: Long  = 0 // Main Search Probes
  var ttHits: Long    = 0 // Main Search Hits
  var qTtProbes: Long = 0 // QSearch Probes
  var qTtHits: Long   = 0 // QSearch Hits

  // move ordering and cutoffs
  var betaCutoffs: Long      = 0
  var firstMoveCutoffs: Long = 0
  var killerCutoffs: Long    = 0
  var historyCutoffs: Long   = 0

  // search features
  var lmrReductions: Long       = 0
  var lmrResearches: Long       = 0
  var aspirationFailLows: Long  = 0
  var aspirationFailHighs: Long = 0
  var pvsReSearches: Long       = 0
  var futilityPrunes: Long      = 0

  // quiescence search specific counts
  var qSearchMaxDepth: Int           = 0
  var qSearchInCheckCount: Long      = 0
  var qSearchCapturesGenerated: Long = 0
  var qSearchMovesSearched: Long     = 0

  def reset(): Unit =
    nodes = 0; qNodes = 0; leafNodes = 0
    ttProbes = 0; ttHits = 0; qTtProbes = 0; qTtHits = 0
    betaCutoffs = 0; firstMoveCutoffs = 0; killerCutoffs = 0; historyCutoffs = 0
    lmrReductions = 0; lmrResearches = 0
    aspirationFailLows = 0; aspirationFailHighs = 0
    pvsReSearches = 0
    qSearchMaxDepth = 0; qSearchInCheckCount = 0
    qSearchCapturesGenerated = 0; qSearchMovesSearched = 0

  def printReport(): Unit =
    val totalNodes = nodes + qNodes
    val qRatio     = if totalNodes > 0 then (qNodes.toDouble / totalNodes) * 100 else 0.0

    println("\n====================== SEARCH STATISTICS ======================")
    println(f"Nodes:     $totalNodes%,d (Main: $nodes%,d | Q: $qNodes%,d | $qRatio%.1f%% Q-Nodes)")

    // tt stats
    printTTStats("Main TT", ttHits, ttProbes)
    printTTStats("QSrch TT", qTtHits, qTtProbes)

    // move ordering
    if betaCutoffs > 0 then
      println("\n--- Move Ordering Efficiency ---")
      println(f"Total Beta Cutoffs: $betaCutoffs%,d")
      printCutoff("PV/Hash Move", firstMoveCutoffs)
      printCutoff("Killer Move", killerCutoffs)
      printCutoff("History Move", historyCutoffs)

      val remaining = betaCutoffs - (firstMoveCutoffs + killerCutoffs + historyCutoffs)
      if remaining > 0 then printCutoff("Late Moves", remaining)

    // search features
    if lmrReductions > 0 || aspirationFailLows > 0 || futilityPrunes > 0 then
      println("\n--- Search Features ---")
      if lmrReductions > 0 then
        val reSearchRate = if lmrReductions > 0 then (lmrResearches.toDouble / lmrReductions) * 100 else 0.0
        println(f"LMR: $lmrReductions%,d reductions (Researched: $lmrResearches%,d | $reSearchRate%.1f%%)")

      if aspirationFailLows > 0 || aspirationFailHighs > 0 then println(f"Aspiration: +$aspirationFailHighs%,d / -$aspirationFailLows%,d")

      if futilityPrunes > 0 then println(f"Futility prunes: $futilityPrunes%,d prunes")

    // q-search
    println("\n--- Quiescence Breakdown ---")
    println(f"Max Depth: $qSearchMaxDepth   | Checks Handled: $qSearchInCheckCount%,d")
    println(f"Captures:  $qSearchCapturesGenerated%,d | Moves Played:   $qSearchMovesSearched%,d")
    if leafNodes > 0 then
      val qPerLeaf = if leafNodes > 0 then qNodes.toDouble / leafNodes else 0.0
      println(f"Avg Q-Nodes per Leaf: $qPerLeaf%.1f")
    println("===============================================================\n")

  private def printTTStats(label: String, hits: Long, probes: Long): Unit =
    val rate = if probes > 0 then (hits.toDouble / probes) * 100 else 0.0
    println(f"$label%-10s $hits%,d / $probes%,d ($rate%.1f%%)")

  private def printCutoff(label: String, count: Long): Unit =
    val rate = if betaCutoffs > 0 then (count.toDouble / betaCutoffs) * 100 else 0.0
    println(f"  $label%-12s $count%,d ($rate%.1f%%)")
