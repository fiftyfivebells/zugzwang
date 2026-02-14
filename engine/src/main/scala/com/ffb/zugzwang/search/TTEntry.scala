package com.ffb.zugzwang.search

import com.ffb.zugzwang.core.{Depth, Ply, Score}
import com.ffb.zugzwang.move.Move

opaque type TTEntry = Long
object TTEntry:
  private val FlagMask  = 0x3L
  private val DepthMask = 0xffL
  private val ScoreMask = 0xffffL
  private val MoveMask  = 0xffffffffL
  private val GenMask   = 0x3fL

  private val ScoreShift = 32
  private val DepthShift = 48
  private val FlagShift  = 56
  private val GenShift   = 58

  // Flag Constants
  val FlagExact: Long = 0L
  val FlagLower: Long = 1L // beta cutoff (stored score is lower bound)
  val FlagUpper: Long = 2L // fail-low (store score is upper bound)

  val None: TTEntry = 0L

  // TODO: replace flag: Long with a defined type
  def apply(move: Move, rawScore: Score, depth: Depth, flag: Long, ply: Ply, gen: Int): TTEntry =
    val storedScore =
      if rawScore > (Score.Checkmate - 1000) then rawScore + ply.value
      else if rawScore < -(Score.Checkmate - 1000) then rawScore - ply.value
      else rawScore

    val moveBits  = move.toLong & MoveMask
    val scoreBits = (storedScore.toLong & ScoreMask) << ScoreShift
    val depthBits = (depth.toLong & DepthMask) << DepthShift
    val flagBits  = (flag & FlagMask) << FlagShift
    val genBits   = (gen.toLong & GenMask) << GenShift

    moveBits | scoreBits | depthBits | flagBits | genBits

  extension (tte: TTEntry)
    inline def isEmpty: Boolean   = tte == 0L
    inline def isDefined: Boolean = tte != 0L

    inline def move: Move = Move.fromInt((tte & MoveMask).toInt)

    inline def depth: Depth =
      val d = ((tte >>> DepthShift) & DepthMask).toInt
      Depth(d)

    inline def flag: Long =
      (tte >>> FlagShift) & FlagMask

    inline def generation: Int =
      ((tte >>> GenShift) & GenMask).toInt

    inline def score(ply: Ply): Score =
      val internalScore = Score((tte >> ScoreShift).toShort.toInt)

      if internalScore > (Score.Checkmate - 1000) then internalScore - ply.value
      else if internalScore < -(Score.Checkmate - 1000) then internalScore + ply.value
      else internalScore

    inline def canCutoff(depth: Depth, alpha: Score, beta: Score, ply: Ply): Boolean =
      if tte.depth < depth then false
      else
        val score = tte.score(ply)

        if tte.flag == FlagExact then true
        else if tte.flag == FlagUpper && score <= alpha then true
        else if tte.flag == FlagLower && score >= beta then true
        else false
