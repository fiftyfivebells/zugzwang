package com.ffb.zugzwang.core

import scala.annotation.targetName

opaque type Node = Long
object Node:
  val zero: Node = 0L

  def apply(in: Long): Node = in

  extension (n: Node)
    inline def value: Long = n

    inline def isZero: Boolean = n == 0

    inline def perSecond(millis: Long): Long =
      if millis > 0 then (n * 1000) / millis else 0

    @targetName("plus")
    inline def +(other: Long): Node = n + other
    inline def +(other: Node): Node = n + other

    @targetName("and")
    inline def &(other: Long): Node = n & other
    inline def &(other: Node): Node = n & other

    inline def asString: String = n.toString
