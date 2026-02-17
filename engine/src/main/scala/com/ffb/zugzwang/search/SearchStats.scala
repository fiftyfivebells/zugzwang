package com.ffb.zugzwang.search

object SearchStats:
  var nodes: Long     = 0
  var qNodes: Long    = 0 // Quiescence search nodes
  var leafNodes: Long = 0
  var ttHits: Long    = 0
  var ttProbes: Long  = 0

  var betaCutoffs: Long           = 0
  var firstMoveCutoffs: Long      = 0
  var killerCutoffs: Long         = 0
  var historyCutoffs: Long        = 0
  var qSearchMaxDepth             = 0
  var qSearchInCheckCount         = 0
  var qSearchCapturesGenerated    = 0
  var qSearchMovesSearched        = 0
  var aspirationFailLows: Long    = 0
  var aspirationFailHighs: Long   = 0
  var pvsReSearches: Long         = 0
  var pvsNullWindowSearches: Long = 0
  var lmrReductions: Long         = 0
  var lmrResearches: Long         = 0

  def reset(): Unit =
    nodes = 0; qNodes = 0; ttHits = 0; ttProbes = 0;
    betaCutoffs = 0; firstMoveCutoffs = 0; killerCutoffs = 0; historyCutoffs = 0
    qSearchCapturesGenerated = 0; qSearchInCheckCount = 0; qSearchMaxDepth = 0
    qSearchMovesSearched = 0; aspirationFailLows = 0; aspirationFailHighs = 0
    pvsReSearches = 0; pvsNullWindowSearches = 0; lmrReductions = 0; lmrResearches = 0

  def printReport(): Unit =
    println()
    println("================ SEARCH STATISTICS ================")
    println(f"Nodes: $nodes%,d (Q-Nodes: $qNodes%,d)")
    if nodes > 0 then println(f"Q-Node Ratio: ${(qNodes.toDouble / nodes) * 100}%.1f%%")
    println(
      f"TT Hits: $ttHits%,d / $ttProbes%,d probes (${if ttProbes > 0 then f"${(ttHits.toDouble / ttProbes) * 100}%.1f" else "0.0"}%%)"
    )

    if betaCutoffs > 0 then
      println("--- Move Ordering Efficiency ---")
      println(f"Total Beta Cutoffs: $betaCutoffs%,d")
      println(f"  First Move (PV/TT): ${firstMoveCutoffs}%,d (${(firstMoveCutoffs.toDouble / betaCutoffs) * 100}%.1f%%)")
      println(f"  Killer Moves:       ${killerCutoffs}%,d (${(killerCutoffs.toDouble / betaCutoffs) * 100}%.1f%%)")
      println(f"  History Moves:      ${historyCutoffs}%,d (${(historyCutoffs.toDouble / betaCutoffs) * 100}%.1f%%)")
      println(f"  LMR Reductions:     ${lmrReductions}%,d")
      println(f"  LMR Researches:     ${lmrResearches}%,d")

    if pvsReSearches > 0 then
      println(f"  PVS Re-searches:         $pvsReSearches%,d")
      println(f"  PVS Null value searches: $pvsReSearches%,d")
    if aspirationFailLows > 0 || aspirationFailHighs > 0 then
      println("--- Aspiration Window Stats ---")
      println(f"  Fail-Lows:  $aspirationFailLows%,d")
      println(f"  Fail-Highs: $aspirationFailHighs%,d")
      println(f"  Total Fails: ${aspirationFailLows + aspirationFailHighs}%,d")
    println(f"  Q-Nodes per Leaf:   ${qNodes.toDouble / leafNodes}%.2f")
    println(f"  Q Search Max Depth: ${qSearchMaxDepth}%,d")
    println(f"  Q Search In Check:  ${qSearchInCheckCount}%,d")
    println(f"  Q Search Captures:  ${qSearchCapturesGenerated}%,d")
    println(f"  Q Search Total:     ${qSearchMovesSearched}%,d")
    println("===================================================")
