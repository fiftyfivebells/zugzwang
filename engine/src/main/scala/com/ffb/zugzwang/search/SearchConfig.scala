package com.ffb.zugzwang.search

import com.ffb.zugzwang.core.TimeManager

object SearchConfig:

  // aspiration Windows
  var aspWindowSize: Int  = 50
  var aspMinDepth: Int    = 5
  var aspMaxAttempts: Int = 3

  // null Move Pruning
  var nmpMinDepth: Int      = 3
  var nmpBaseReduction: Int = 2
  var nmpDeepReduction: Int = 3
  var nmpDeepThreshold: Int = 6

  // futility Pruning (move-loop)
  var fpMaxDepth: Int       = 3
  var fpMarginPerDepth: Int = 150

  // reverse futility pruning (pre-move-loop)
  var rfpMaxDepth: Int       = 3
  var rfpMarginPerDepth: Int = 80

  // razoring
  var razorMaxDepth: Int = 2
  var razorMarginD1: Int = 300
  var razorMarginD2: Int = 600

  // late move reductions
  var lmrMinDepth: Int       = 3
  var lmrMinMoveIndex: Int   = 3
  var lmrDivisor: Double     = 2.5
  var lmrHistoryDivisor: Int = 8192

  // late move pruning
  var lmpMaxDepth: Int          = 3
  var lmpThresholds: Array[Int] = Array(0, 8, 12, 20, 28)

  // check extensions
  var checkExtPvOnly: Boolean = true

  // internal iterative reduction
  var iirMinDepth: Int = 4

  // history tables
  var historyMax: Int     = 16384
  var captureHistMax: Int = 16384

  // quiescence search
  var qFutilityMargin: Int = 150
  var qMaxDepth: Int       = 10

  // pre-computed lmr table
  var lmrTable: Array[Array[Int]] = computeLmrTable()

  private def computeLmrTable(): Array[Array[Int]] =
    val maxDepth = 128
    val maxMoves = 128
    val table    = Array.ofDim[Int](maxDepth, maxMoves)
    var d        = 1
    while d < maxDepth do
      var m = 1
      while m < maxMoves do
        table(d)(m) = math.max(1, (math.log(d) * math.log(m) / lmrDivisor).floor.toInt)
        m += 1
      d += 1
    table

  def recomputeLmrTable(): Unit =
    lmrTable = computeLmrTable()

  // uci setoption
  def setOption(name: String, value: String): Boolean =
    try
      name.toLowerCase match
        case "aspwindowsize"       => aspWindowSize = value.toInt; true
        case "aspmindepth"         => aspMinDepth = value.toInt; true
        case "aspmaxattempts"      => aspMaxAttempts = value.toInt; true
        case "nmpmindepth"         => nmpMinDepth = value.toInt; true
        case "nmpbasereduction"    => nmpBaseReduction = value.toInt; true
        case "nmpdeepreduction"    => nmpDeepReduction = value.toInt; true
        case "nmpdeepthreshold"    => nmpDeepThreshold = value.toInt; true
        case "fpmaxdepth"          => fpMaxDepth = value.toInt; true
        case "fpmarginperdepth"    => fpMarginPerDepth = value.toInt; true
        case "rfpmaxdepth"         => rfpMaxDepth = value.toInt; true
        case "rfpmarginperdepth"   => rfpMarginPerDepth = value.toInt; true
        case "razormaxdepth"       => razorMaxDepth = value.toInt; true
        case "lmrdivisor"          => lmrDivisor = value.toDouble; recomputeLmrTable(); true
        case "lmrhistorydivisor"   => lmrHistoryDivisor = value.toInt; true
        case "historymax"          => historyMax = value.toInt; true
        case "capturehistmax"      => captureHistMax = value.toInt; true
        case "qfutilitymargin"     => qFutilityMargin = value.toInt; true
        case "qmaxdepth"           => qMaxDepth = value.toInt; true
        case "tmstabilitycenter"   => TimeManager.tmStabilityCenter = value.toDouble; true
        case "tminstabilitycoeff"  => TimeManager.tmInstabilityCoeff = value.toDouble; true
        case "tminstabilitybase"   => TimeManager.tmInstabilityBase = value.toDouble; true
        case "tmhardlimitfraction" => TimeManager.tmHardLimitFraction = value.toInt; true
        case _                     => false
    catch case _: NumberFormatException => false
