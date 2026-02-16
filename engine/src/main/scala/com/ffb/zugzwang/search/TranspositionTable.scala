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

  private var generation: Int = 0

  def incrementGeneration(): Unit =
    generation = (generation + 1) & 0x3f

  def currentGeneration: Int = generation

  def probe(zobristHash: ZobristHash): TTEntry =
    SearchStats.ttProbes += 1
    val index = (zobristHash.value & mask).toInt

    if keys(index) == zobristHash.value then return data(index).asInstanceOf[TTEntry]

    if index + 1 < numEntries && keys(index + 1) == zobristHash.value then return data(index + 1).asInstanceOf[TTEntry]

    TTEntry.None

  def store(zobristHash: ZobristHash, move: Move, score: Score, depth: Depth, flag: Long, ply: Ply): Unit =
    val index  = (zobristHash.value & mask).toInt
    val packed = TTEntry(move, score, depth, flag, ply, generation)

    if index + 1 >= numEntries then
      keys(index) = zobristHash.value
      data(index) = packed.asInstanceOf[Long]
      return

    if keys(index) == zobristHash.value then
      data(index) = packed.asInstanceOf[Long]
      return

    if keys(index + 1) == zobristHash.value then
      data(index + 1) = packed.asInstanceOf[Long]
      return

    val entry0 = data(index).asInstanceOf[TTEntry]
    if entry0.isEmpty || entry0.depth.value <= depth.value || entry0.generation != generation then
      keys(index) = zobristHash.value
      data(index) = packed.asInstanceOf[Long]
    else
      keys(index + 1) = zobristHash.value
      data(index + 1) = packed.asInstanceOf[Long]

  def clear(): Unit =
    java.util.Arrays.fill(keys, 0L)
    java.util.Arrays.fill(data, 0L)
    generation = 0
