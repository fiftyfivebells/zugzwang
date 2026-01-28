package com.ffb.zugzwang.core

import scala.annotation.targetName

opaque type Ply = Int
object Ply:
  val base: Ply = 1

  def apply(in: Int): Ply = in

  extension (p: Ply)
    inline def value: Int = p

    inline def isZero: Boolean = p == 0

    @targetName("plus")
    inline def +(other: Int): Ply = p + other
    inline def +(other: Ply): Ply = p + other
