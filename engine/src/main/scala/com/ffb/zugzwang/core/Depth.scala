package com.ffb.zugzwang.core

import scala.annotation.targetName

opaque type Depth = Int
object Depth:
  def apply(in: Int): Depth = in

  extension (d: Depth)
    inline def value: Int = d

    inline def isZero: Boolean = d == 0

    @targetName("plus")
    inline def +(other: Int): Depth   = d + other
    inline def +(other: Depth): Depth = d + other

    @targetName("minus")
    inline def -(other: Int): Depth   = d - other
    inline def -(other: Depth): Depth = d - other

    @targetName("lessThan")
    inline def <(other: Int): Boolean   = d < other
    inline def <(other: Depth): Boolean = d < other

    @targetName("greaterThan")
    inline def >(other: Int): Boolean   = d > other
    inline def >(other: Depth): Boolean = d > other
