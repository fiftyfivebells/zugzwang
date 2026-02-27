package com.ffb.zugzwang.move

import com.ffb.zugzwang.chess.MutablePosition
import com.ffb.zugzwang.chess.zobrist.Zobrist
import com.ffb.zugzwang.search.SearchMoveGen

object Perft:

  /** Count leaf nodes. Used by UciMain for `go perft <depth>`. */
  def perft(position: MutablePosition, depth: Int): Long =
    if depth == 0 then 1L
    else
      val ml    = MoveList(256)
      val mover = position.activeSide
      var nodes = 0L

      SearchMoveGen.fillMoveList(position, ml)
      val arr = ml.toArray

      var i = 0
      while i < arr.length do
        val move = arr(i)
        position.applyMove(move)
        if !position.isSquareAttacked(position.kingSq(mover.ordinal), position.activeSide) then nodes += perft(position, depth - 1)
        position.unapplyMove(move)
        i += 1

      nodes

  /**
   * Print per-move node counts at the root. Used by UciMain for `go perft
   * <depth>`.
   */
  def divide(position: MutablePosition, depth: Int): Long =
    val ml    = MoveList(256)
    val mover = position.activeSide
    var total = 0L

    SearchMoveGen.fillMoveList(position, ml)
    val arr = ml.toArray

    var i = 0
    while i < arr.length do
      val move = arr(i)
      position.applyMove(move)
      if !position.isSquareAttacked(position.kingSq(mover.ordinal), position.activeSide) then
        val nodes = perft(position, depth - 1)
        println(s"${move.toUci}: $nodes")
        total += nodes
      position.unapplyMove(move)
      i += 1

    println(s"\nTotal: $total")
    total

  /**
   * Debug tool: validates Zobrist hash integrity at every node. Not exposed
   * through UCI. Call directly when debugging hash corruption.
   */
  def zobristDebugPerft(position: MutablePosition, depth: Int): Long =
    val currentHash    = position.zobristHash
    val calculatedHash = Zobrist.compute(position)

    if currentHash != calculatedHash then
      println(s"CRITICAL: Hash mismatch at depth $depth")
      println(s"Incremental: $currentHash")
      println(s"Computed:    $calculatedHash")
      throw new RuntimeException("Hash corruption detected")

    if depth == 0 then return 1L

    val ml    = MoveList(256)
    val mover = position.activeSide
    var nodes = 0L

    SearchMoveGen.fillMoveList(position, ml)
    val arr = ml.toArray

    var i = 0
    while i < arr.length do
      val move        = arr(i)
      val preMoveHash = position.zobristHash
      position.applyMove(move)
      if !position.isSquareAttacked(position.kingSq(mover.ordinal), position.activeSide) then
        nodes += zobristDebugPerft(position, depth - 1)
      position.unapplyMove(move)
      if position.zobristHash != preMoveHash then
        println(s"CRITICAL: Hash not restored after unapplyMove on ${move.toUci}")
        throw new RuntimeException("Unmake corruption detected")
      i += 1

    nodes
