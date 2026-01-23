package com.ffb.zugzwang.move

import com.ffb.zugzwang.chess.{Color, GameState, MutablePosition, PositionState}
import com.ffb.zugzwang.rules.Rules

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

object Perft:

  def perftSequential(state: GameState, depth: Int): Long =
    @tailrec
    def loop(stack: List[(GameState, Int)], acc: Long): Long = stack match
      case Nil => acc
      case (s, d) :: rest =>
        if d == 0 then loop(rest, acc + 1)
        else
          val nextStates = MoveGenerator.legalMoves(s) map { move =>
            (Rules.applyMove(s, move), d - 1)
          }
          loop(nextStates ++ rest, acc)

    loop(List((state, depth)), 0)

  def perft(state: GameState, depth: Int): Long =
    if depth == 0 then 1L
    else
      MoveGenerator
        .legalMoves(state)
        .toList
        .par
        .map { move =>
          val newState = Rules.applyMove(state, move)
          perftSequential(newState, depth - 1)
        }
        .sum

  def perftBasic(state: GameState, depth: Int): Long =
    if depth == 0 then 1
    else
      MoveGenerator
        .legalMoves(state)
        .map(move => perftBasic(Rules.applyMove(state, move), depth - 1))
        .sum

  def perftMutable(position: MutablePosition, depth: Int): Long =
    if depth == 0 then 1
    else
      // Scratch per ply so recursion doesn't overwrite buffers.
      val moveLists = Array.fill(depth + 1)(MoveList(256))
      val undos     = Array.fill(depth + 1)(new PositionState)

      def rec(d: Int): Long =
        if d == 0 then 1L
        else
          val ml = moveLists(d)
          ml.clear
          val arr = MoveGenerator.pseudoLegalMoves(position).toArray

          val n = arr.size

          var i     = 0
          var nodes = 0L

          while i < n do
            val m = arr(i)
            val u = undos(d)

            // Side to move BEFORE make is the mover.
            val mover: Color = position.activeSide

            position.applyMove(m, u)

            // After makeMove, sideToMove has flipped.
            // The mover's king must not be attacked by the new side to move (the opponent).
            val king    = position.kingSq(mover.ordinal)
            val illegal = position.isSquareAttacked(king, position.activeSide)

            if !illegal then nodes += rec(d - 1)

            position.unapplyMove(m, u)
            i += 1

          nodes

      rec(depth)

  def perftTrace(state: GameState, depth: Int, trace: List[Move] = Nil): Long =
    if depth == 0 then
      println("TRACE: " + trace.reverse.map(_.toUci).mkString(" "))
      1
    else
      MoveGenerator
        .legalMoves(state)
        .map(move => perftTrace(Rules.applyMove(state, move), depth - 1, move :: trace))
        .sum

  def divide(state: GameState, depth: Int): Long =
    val moves = MoveGenerator.legalMoves(state)
    var total = 0L

    for move <- moves do
      val nextState = Rules.applyMove(state, move)
      val nodes     = perftBasic(nextState, depth - 1)
      println(s"${move.toUci}: $nodes")
      total += nodes

    println(s"Total Nodes: $total")
    total

  def divideMutable(position: MutablePosition, depth: Int): Long =
    if depth == 0 then
      println("Total Nodes: 1")
      return 1L

    // We need move lists and undo objects for the recursion
    // MoveLists are indexed by depth to avoid overwriting at different plys
    val moveLists = Array.fill(depth + 1)(MoveList(256))
    val undos     = Array.fill(depth + 1)(new PositionState)

    var totalNodes = 0L

    // 1. Get moves for the ROOT position
    val rootMoves = MoveGenerator.pseudoLegalMoves(position).toArray
    val mover     = position.activeSide

    for move <- rootMoves do
      val u = undos(depth)

      // 2. Apply the move
      position.applyMove(move, u)

      // 3. Verify legality (King not left in check)
      val king    = position.kingSq(mover.ordinal)
      val illegal = position.isSquareAttacked(king, position.activeSide)

      if !illegal then
        // 4. Run the recursive perft for the remaining depth
        val nodes = recMutable(position, depth - 1, moveLists, undos)
        println(s"${move.toUci}: $nodes")
        totalNodes += nodes

      // 5. Always unapply
      position.unapplyMove(move, u)

    println(s"Total Nodes: $totalNodes")
    totalNodes

  // Helper function for the recursive part
  private def recMutable(
    position: MutablePosition,
    d: Int,
    moveLists: Array[MoveList],
    undos: Array[PositionState]
  ): Long =
    if d == 0 then 1L
    else
      val ml = moveLists(d)
      ml.clear

      // Generate pseudo-legal moves for this depth
      val moves = MoveGenerator.pseudoLegalMoves(position).toArray
      val mover = position.activeSide
      var nodes = 0L
      var i     = 0

      while i < moves.length do
        val m = moves(i)
        val u = undos(d)

        position.applyMove(m, u)

        val king = position.kingSq(mover.ordinal)
        if !position.isSquareAttacked(king, position.activeSide) then nodes += recMutable(position, d - 1, moveLists, undos)

        position.unapplyMove(m, u)
        i += 1

      nodes
