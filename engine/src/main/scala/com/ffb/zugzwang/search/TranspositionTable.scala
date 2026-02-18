package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.zobrist.ZobristHash
import com.ffb.zugzwang.core.{Depth, Ply, Score}
import com.ffb.zugzwang.move.Move

import java.util.Arrays

final class TranspositionTable(sizeInMb: Int):
  private val entrySize  = 16
  private val numEntries = Integer.highestOneBit((sizeInMb * 1024 * 1024) / entrySize)
  private val mask       = numEntries - 1

  private val keys = new Array[Long](numEntries)
  private val data = new Array[Long](numEntries)

  private var generation: Int = 0

  def incrementGeneration(): Unit =
    generation = (generation + 1) & 0x3f // Wrap at 64

  def probe(zobristHash: ZobristHash): TTEntry =
    val index = (zobristHash.value & mask).toInt
    val peer  = index ^ 1

    if keys(index) == zobristHash.value then return data(index).asInstanceOf[TTEntry]
    if keys(peer) == zobristHash.value then return data(peer).asInstanceOf[TTEntry]

    TTEntry.None

  def store(zobristHash: ZobristHash, move: Move, score: Score, depth: Depth, flag: Long, ply: Ply): Unit =
    val index = (zobristHash.value & mask).toInt
    val peer  = index ^ 1

    val newEntry = TTEntry(move, score, depth, flag, ply, generation)

    if keys(index) == zobristHash.value then
      val existing = data(index).asInstanceOf[TTEntry]
      if shouldReplace(existing, depth, flag) then data(index) = newEntry.asInstanceOf[Long]
      return

    if keys(peer) == zobristHash.value then
      val existing = data(peer).asInstanceOf[TTEntry]
      if shouldReplace(existing, depth, flag) then data(peer) = newEntry.asInstanceOf[Long]
      return

    val entry1 = data(index).asInstanceOf[TTEntry]
    val entry2 = data(peer).asInstanceOf[TTEntry]

    val score1 = calculateQuality(entry1)
    val score2 = calculateQuality(entry2)

    if score1 <= score2 then
      keys(index) = zobristHash.value
      data(index) = newEntry.asInstanceOf[Long]
    else
      keys(peer) = zobristHash.value
      data(peer) = newEntry.asInstanceOf[Long]

  private def shouldReplace(existing: TTEntry, newDepth: Depth, newFlag: Long): Boolean =
    // always update if the stored entry is from an old generation
    if existing.generation != generation then return true

    // always update if we've already searched deeper or equal depth
    if newDepth.value >= existing.depth.value then return true

    // special case: uf depths are close, prefer exact scores over bounds
    if existing.flag != TTEntry.FlagExact && newFlag == TTEntry.FlagExact then
      // allow slight depth degradation (for example, 2 ply) to store an exact score
      if newDepth.value >= existing.depth.value - 2 then return true

    false

  private def calculateQuality(entry: TTEntry): Int =
    if entry.isEmpty then return -1

    var score = 0
    if entry.generation == generation then score += 1000
    score += entry.depth.value * 2
    if entry.flag == TTEntry.FlagExact then score += 1

    score

  def clear(): Unit =
    Arrays.fill(keys, 0L)
    Arrays.fill(data, 0L)
    generation = 0
