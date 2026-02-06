package com.ffb.zugzwang.move
import com.ffb.zugzwang.chess.zobrist.Zobrist
import com.ffb.zugzwang.chess.{Color, GameState, MutablePosition}
import com.ffb.zugzwang.move.{Move, MoveGenerator, MoveList}
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
      val moveLists = Array.fill(depth + 1)(MoveList(256))

      def rec(d: Int): Long =
        if d == 0 then 1L
        else
          val ml = moveLists(d)
          ml.clear
          val arr = MoveGenerator.pseudoLegalMovesMutable(position).toArray

          val n = arr.size

          var i     = 0
          var nodes = 0L

          while i < n do
            val m = arr(i)

            // Side to move BEFORE make is the mover.
            val mover: Color = position.activeSide

            position.applyMove(m)

            // After makeMove, sideToMove has flipped.
            // The mover's king can't be attacked by the new side to move (the opponent).
            val king    = position.kingSq(mover.ordinal)
            val illegal = position.isSquareAttacked(king, position.activeSide)

            if !illegal then nodes += rec(d - 1)

            position.unapplyMove(m)
            i += 1

          nodes

      rec(depth)

  // perft debugger: validates Zobrist Hash integrity at every node and every unmake
  def zobristDebugPerft(position: MutablePosition, depth: Int): Long =
    val currentHash    = position.zobristHash
    val calculatedHash = Zobrist.compute(position)

    if currentHash != calculatedHash then
      println(s"CRITICAL: Hash Mismatch at depth $depth")
      println(s"Fen: ${GameState.from(position).toFen}")
      println(s"Current (Incremental): $currentHash")
      println(s"Calculated (Scratch):  $calculatedHash")
      throw new RuntimeException("Hash Corruption Detected")

    if depth == 0 then return 1L

    val moves = MoveGenerator.pseudoLegalMovesMutable(position).toArray
    var nodes = 0L
    var i     = 0
    val mover = position.activeSide

    while i < moves.length do
      val m = moves(i)

      val preMoveHash = position.zobristHash

      position.applyMove(m)

      val king    = position.kingSq(mover.ordinal)
      val illegal = position.isSquareAttacked(king, position.activeSide)

      if !illegal then nodes += zobristDebugPerft(position, depth - 1)

      position.unapplyMove(m)

      if position.zobristHash != preMoveHash then
        println("CRITICAL: Hash not restored after unapplyMove!")
        println(s"Move: ${m.toUci}")
        println(s"Expected: $preMoveHash")
        println(s"Actual:   ${position.zobristHash}")
        println(s"Fen: ${GameState.from(position).toFen}")
        throw new RuntimeException("Unmake Corruption Detected")

      i += 1

    nodes

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

    // MoveLists are indexed by depth to avoid overwriting at different plys
    val moveLists = Array.fill(depth + 1)(MoveList(256))

    var totalNodes = 0L

    val rootMoves = MoveGenerator.pseudoLegalMovesMutable(position).toArray
    val mover     = position.activeSide

    for move <- rootMoves do

      position.applyMove(move)

      val king    = position.kingSq(mover.ordinal)
      val illegal = position.isSquareAttacked(king, position.activeSide)

      if !illegal then
        val nodes = recMutable(position, depth - 1, moveLists)
        println(s"${move.toUci}: $nodes")
        totalNodes += nodes

      position.unapplyMove(move)

    println(s"\nTotal Nodes: $totalNodes")
    totalNodes

  private def recMutable(
    position: MutablePosition,
    d: Int,
    moveLists: Array[MoveList]
  ): Long =
    if d == 0 then 1L
    else
      val ml = moveLists(d)
      ml.clear

      val moves = MoveGenerator.pseudoLegalMovesMutable(position).toArray
      val mover = position.activeSide
      var nodes = 0L
      var i     = 0

      while i < moves.length do
        val m = moves(i)

        position.applyMove(m)

        val king = position.kingSq(mover.ordinal)
        if !position.isSquareAttacked(king, position.activeSide) then nodes += recMutable(position, d - 1, moveLists)

        position.unapplyMove(m)
        i += 1

      nodes
