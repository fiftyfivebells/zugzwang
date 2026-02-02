package com.ffb.zugzwang.core

import scala.annotation.targetName

opaque type SearchTime = Long
object SearchTime:
  val maxTime: SearchTime = Long.MaxValue
  val minTime: SearchTime = Long.MinValue

  inline def currentTime: SearchTime = System.nanoTime() / 1000000 // time in ms

  def apply(in: Long): SearchTime = in

  extension (st: SearchTime)
    inline def toLong: Long = st

    inline def isMax: Boolean = st == maxTime
    inline def isMin: Boolean = st == minTime

    @targetName("greaterThanEqualLong")
    inline def >=(other: Long): Boolean = st >= other
    @targetName("greaterThanEqualSearchTime")
    inline def >=(other: SearchTime): Boolean = st >= other

    @targetName("greaterThanLong")
    inline def >(other: Long): Boolean = st > other
    @targetName("greaterThanSearchTime")
    inline def >(other: SearchTime): Boolean = st > other

    @targetName("lessThanEqualLong")
    inline def <=(other: Long): Boolean = st <= other
    @targetName("lessThanEqualSearchTime")
    inline def <=(other: SearchTime): Boolean = st <= other

    @targetName("minusLong")
    inline def -(other: Long): SearchTime = st - other
    @targetName("minusSearchTime")
    inline def -(other: SearchTime): SearchTime = st - other

    @targetName("plusLong")
    inline def +(other: Long): SearchTime = st + other
    @targetName("plusSearchTime")
    inline def +(other: SearchTime): SearchTime = st + other
