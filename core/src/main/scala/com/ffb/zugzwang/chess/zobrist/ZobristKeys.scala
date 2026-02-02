package com.ffb.zugzwang.chess.zobrist

object ZobristKeys:
  private final class SplitMix64(private var x: Long):
    inline def nextLong: Long =
      x += 0x9e3779b97f4a7c15L
      var z = x
      z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
      z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
      z ^ (z >>> 31)

  private val rng = SplitMix64(0xdeadbeef) // TODO: do I need a better rng seed?

  val pieceSquare: Array[Array[Long]] = Array.fill(12)(Array.fill(64)(rng.nextLong))

  val castling: Array[Long] = Array.fill(16)(rng.nextLong)

  val epFile: Array[Long] = Array.fill(8)(rng.nextLong)

  val sideToMove: Long = rng.nextLong
