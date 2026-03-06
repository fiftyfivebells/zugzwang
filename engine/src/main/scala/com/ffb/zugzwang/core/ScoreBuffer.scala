package com.ffb.zugzwang.core

opaque type ScoreBuffer = Array[Score]
object ScoreBuffer:
  inline def initial: ScoreBuffer            = new Array[Score](256)
  inline def initialize(i: Int): ScoreBuffer = new Array[Score](i)

  extension (sb: ScoreBuffer)
    inline def setScore(i: Int, score: Score): Unit = sb(i) = score
    inline def getScore(i: Int): Score              = sb(i)
