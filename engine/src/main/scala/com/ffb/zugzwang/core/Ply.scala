package com.ffb.zugzwang.core

import scala.annotation.targetName

opaque type Ply = Int
object Ply:
  val base: Ply = 1

  def apply(in: Int): Ply = in

  extension (p: Ply)
    inline def value: Int = p

    inline def isZero: Boolean = p == 0

    @targetName("plusInt")
    inline def +(other: Int): Ply = p + other
    @targetName("plusPly")
    inline def +(other: Ply): Ply = p + other

    @targetName("lessThanInt")
    inline def <(other: Int): Boolean = p < other
    @targetName("lessThanPly")
    inline def <(other: Ply): Boolean = p < other

    @targetName("lessThanEqualInt")
    inline def <=(other: Int): Boolean = p <= other
    @targetName("lessThanEqualPly")
    inline def <=(other: Ply): Boolean = p <= other

    @targetName("greaterThanInt")
    inline def >(other: Int): Boolean = p > other
    @targetName("greaterThanPly")
    inline def >(other: Ply): Boolean = p > other

    @targetName("greaterThanEqualInt")
    inline def >=(other: Int): Boolean = p >= other
    @targetName("greaterThanEqualPly")
    inline def >=(other: Ply): Boolean = p >= other
