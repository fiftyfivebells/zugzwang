package com.ffb.zugzwang.core

import scala.annotation.targetName

opaque type SearchTime = Long
object SearchTime:
  val maxTime: SearchTime = Long.MaxValue
  val minTime: SearchTime = Long.MinValue

  inline def currentTime: SearchTime = System.currentTimeMillis

  extension (st: SearchTime)
    inline def toLong: Long = st

    @targetName("greaterThanEqual")
    inline def >=(other: Long): Boolean       = st >= other
    inline def >=(other: SearchTime): Boolean = st >= other

    @targetName("greaterThan")
    inline def >(other: Long): Boolean       = st > other
    inline def >(other: SearchTime): Boolean = st > other

    @targetName("lessThanEqual")
    inline def <=(other: Long): Boolean       = st <= other
    inline def <=(other: SearchTime): Boolean = st <= other

    @targetName("minus")
    inline def -(other: Long): SearchTime       = st - other
    inline def -(other: SearchTime): SearchTime = st - other

    @targetName("plus")
    inline def +(other: Long): SearchTime       = st + other
    inline def +(other: SearchTime): SearchTime = st + other
