package com.ffb.zugzwang.chess.zobrist

import scala.annotation.targetName

opaque type ZobristHash = Long
object ZobristHash:
  inline def apply(in: Long): ZobristHash = in

  val Empty: ZobristHash = 0L

  extension (zh: ZobristHash)
    inline def value: Long = zh

    inline def isEqual(other: ZobristHash): Boolean = zh == other

    @targetName("xorLong")
    inline def ^(other: Long): ZobristHash = ZobristHash(zh ^ other)

    @targetName("xorHas")
    inline def ^(other: ZobristHash) = ZobristHash(zh ^ other)
