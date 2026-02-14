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
    if keys(index) == zobristHash.value then data(index).asInstanceOf[TTEntry]
    else TTEntry.None

  def store(zobristHash: ZobristHash, move: Move, score: Score, depth: Depth, flag: Long, ply: Ply): Unit =
    val index = (zobristHash.value & mask).toInt

    val existing     = data(index).asInstanceOf[TTEntry]
    val samePosition = keys(index) == zobristHash.value
    val shouldReplace =
      if samePosition then depth >= existing.depth
      else
        val existingScore = existing.depth.value + (if existing.generation == generation then 8 else 0)
        val newScore      = depth.value + 8
        existingScore <= newScore

    val packed = TTEntry(move, score, depth, flag, ply, generation)
    if existing.isEmpty || shouldReplace then
      keys(index) = zobristHash.value
      data(index) = packed.asInstanceOf[Long]

  def clear(): Unit =
    java.util.Arrays.fill(keys, 0L)
    java.util.Arrays.fill(data, 0L)
