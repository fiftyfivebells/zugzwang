package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.zobrist.ZobristHash
import com.ffb.zugzwang.core.{Depth, Ply, Score}
import com.ffb.zugzwang.move.Move

final class TranspositionTable(sizeInMb: Int):
  private val entrySize  = 16
  private val numEntries = Integer.highestOneBit((sizeInMb * 1024 * 1024) / entrySize)
  private val mask       = numEntries - 1

  private val keys = new Array[Long](numEntries)
  private val data = new Array[Long](numEntries)

  def probe(zobristHash: ZobristHash): TTEntry =
    val index = (zobristHash.value & mask).toInt
    if keys(index) == zobristHash.value then data(index).asInstanceOf[TTEntry]
    else TTEntry.None

  def store(zobristHash: ZobristHash, move: Move, score: Score, depth: Depth, flag: Long, ply: Ply): Unit =
    val index = (zobristHash.value & mask).toInt

    val existing = data(index).asInstanceOf[TTEntry]

    if keys(index) != zobristHash.value || depth >= existing.depth then

      val packed = TTEntry(move, score, depth, flag, ply)

      keys(index) = zobristHash.value
      data(index) = packed.asInstanceOf[Long]

  def clear(): Unit =
    java.util.Arrays.fill(keys, 0L)
    java.util.Arrays.fill(data, 0L)
