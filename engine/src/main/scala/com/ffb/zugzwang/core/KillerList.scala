package com.ffb.zugzwang.core

import com.ffb.zugzwang.move.Move

opaque type KillersList = Array[Killers]
object KillersList:
  inline def initialize(dim: Int): KillersList = Array.fill(dim)(Killers.empty)

  extension (kl: KillersList)
    inline def basePly: Killers = atPly(Ply(0))

    inline def atPly(ply: Ply): Killers =
      val p = ply.toInt
      if p < kl.length then kl(p) else Killers.empty

    inline def updateFirst(ply: Ply, move: Move): Unit =
      kl(ply.toInt).updateFirst(move)

    inline def getFirst(ply: Ply): Move = kl(ply.toInt).first

    inline def updateSecond(ply: Ply, move: Move): Unit =
      kl(ply.toInt).updateSecond(move)

    inline def getSecond(ply: Ply): Move = kl(ply.toInt).second

    inline def insertMove(ply: Ply, move: Move): Unit = kl(ply.toInt).insertMove(move)
