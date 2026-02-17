package com.ffb.zugzwang.core

import scala.annotation.targetName

opaque type Depth = Int
object Depth:
  def apply(in: Int): Depth = in

  val Zero: Depth = 0

  extension (d: Depth)
    inline def value: Int   = d
    inline def toLong: Long = d.toLong

    inline def isZero: Boolean = d == 0

    @targetName("plusInt")
    inline def +(other: Int): Depth = d + other
    @targetName("plusDepth")
    inline def +(other: Depth): Depth = d + other

    @targetName("minusInt")
    inline def -(other: Int): Depth = d - other
    @targetName("minusDepth")
    inline def -(other: Depth): Depth = d - other

    @targetName("lessThanInt")
    inline def <(other: Int): Boolean = d < other
    @targetName("lessThanDepth")
    inline def <(other: Depth): Boolean = d < other

    @targetName("lessThanEqualInt")
    inline def <=(other: Int): Boolean = d <= other
    @targetName("lessThanEqualDepth")
    inline def <=(other: Depth): Boolean = d <= other

    @targetName("greaterThanInt")
    inline def >(other: Int): Boolean = d > other
    @targetName("greaterThanDepth")
    inline def >(other: Depth): Boolean = d > other

    @targetName("greaterThanEqualInt")
    inline def >=(other: Int): Boolean = d >= other
    @targetName("greaterThanEqualDepth")
    inline def >=(other: Depth): Boolean = d >= other
