package com.ffb.zugzwang.core

import scala.annotation.targetName

opaque type Score = Int
object Score:
  val Checkmate: Score = Score(30000)
  val Infinity: Score  = Score(32000)
  val Stalemate: Score = Score(0)
  val Draw: Score      = Score(0)
  val DrawBias: Score  = Score(1)
  val Zero: Score      = Score(0)

  def apply(in: Int): Score = in

  def max(a: Score, b: Score): Score =
    if a >= b then a else b

  def min(a: Score, b: Score): Score =
    if a <= b then a else b

  extension (s: Score)
    inline def value: Int = s

    inline def toLong: Long = s.toLong

    @targetName("plusInt")
    inline def +(other: Int): Score = s + other
    @targetName("plusScore")
    inline def +(other: Score): Score = s + other

    @targetName("minusInt")
    inline def -(other: Int): Score = s - other
    @targetName("minusScore")
    inline def -(other: Score): Score = s - other

    @targetName("timesInt")
    inline def *(other: Int): Score = s * other
    @targetName("timesScore")
    inline def *(other: Score): Score = s * other

    @targetName("divideInt")
    inline def /(other: Int): Score = s / other
    @targetName("divideScore")
    inline def /(other: Score): Score = s / other

    @targetName("greaterThanEqualInt")
    inline def >=(other: Int): Boolean = s >= other
    @targetName("greaterThanEqualScore")
    inline def >=(other: Score): Boolean = s >= other

    @targetName("greaterThanInt")
    inline def >(other: Int): Boolean = s > other
    @targetName("greaterThanScore")
    inline def >(other: Score): Boolean = s > other

    @targetName("lessThanEqualInt")
    inline def <=(other: Int): Boolean = s <= other
    @targetName("lessThanEqualScore")
    inline def <=(other: Score): Boolean = s <= other

    @targetName("lessThanInt")
    inline def <(other: Int): Boolean = s < other
    @targetName("lessThanScore")
    inline def <(other: Score): Boolean = s < other

    inline def unary_- : Score = apply(-s)

    inline def format: String =
      val MateThreshold = Checkmate - 1000

      if s > MateThreshold then
        val pliesToMate = Checkmate - s
        val movesToMate = (pliesToMate + 1) / 2
        s"mate $movesToMate"
      else if s < -MateThreshold then
        val pliesToMate = Checkmate + s // score is negative here
        val movesToMate = (pliesToMate + 1) / 2
        s"mate -$movesToMate"
      else s"cp $s"
