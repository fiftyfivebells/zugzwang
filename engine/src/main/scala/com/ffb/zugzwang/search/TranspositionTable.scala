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

    val newEntry     = TTEntry(move, score, depth, flag, ply, generation)
    val newEntryLong = newEntry.asInstanceOf[Long]

    // if we find the entry, update it exactly.
    // keeps the entry fresh
    if keys(index) == zobristHash.value then
      data(index) = newEntryLong
      return

    if keys(peer) == zobristHash.value then
      data(peer) = newEntryLong
      return

    // collision: need to evict something
    // slot 0 (index) stores the deepest search
    // slot 1 (peer) stores newest search
    val entry0 = data(index).asInstanceOf[TTEntry]

    if depth.value >= entry0.depth.value then
      keys(index) = zobristHash.value
      data(index) = newEntryLong
    else
      keys(peer) = zobristHash.value
      data(peer) = newEntryLong

  def clear(): Unit =
    Arrays.fill(keys, 0L)
    Arrays.fill(data, 0L)
    generation = 0
