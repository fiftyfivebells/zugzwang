package com.ffb.zugzwang.core

import com.ffb.zugzwang.move.Move

opaque type KillersList = Array[Killers]
object KillersList:
  inline def initialize(dim: Int): KillersList = Array.fill(dim)(Killers.empty)

  extension (kl: KillersList)
    inline def basePly: Killers = atPly(Ply(0))

    inline def atPly(ply: Ply): Killers =
      val p = ply.asInt
      if p < kl.length then kl(p) else Killers.empty

    inline def updateFirst(ply: Ply, move: Move): Unit =
      kl(ply.asInt).updateFirst(move)

    inline def getFirst(ply: Ply): Move = kl(ply.asInt).first

    inline def updateSecond(ply: Ply, move: Move): Unit =
      kl(ply.asInt).updateSecond(move)

    inline def getSecond(ply: Ply): Move = kl(ply.asInt).second

    inline def insertMove(ply: Ply, move: Move): Unit = kl(ply.asInt).insertMove(move)
