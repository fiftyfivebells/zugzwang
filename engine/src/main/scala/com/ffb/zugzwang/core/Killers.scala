package com.ffb.zugzwang.core

import com.ffb.zugzwang.move.Move

opaque type Killers = Array[Move]
object Killers:
  inline def empty: Killers = Array.fill(2)(Move.None)

  extension (k: Killers)
    inline def first: Move  = if k.length > 0 then k(0) else Move.None
    inline def second: Move = if k.length > 1 then k(1) else Move.None

    inline def updateFirst(move: Move): Unit  = k(0) = move
    inline def updateSecond(move: Move): Unit = k(1) = move

    inline def insertMove(move: Move): Unit =
      updateSecond(k.first)
      updateFirst(move)

    inline def doesContain(move: Move): Boolean = k.contains(move)
