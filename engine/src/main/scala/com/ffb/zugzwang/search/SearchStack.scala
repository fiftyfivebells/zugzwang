package com.ffb.zugzwang.search

import com.ffb.zugzwang.chess.Piece
import com.ffb.zugzwang.core.{Depth, Ply, Score}
import com.ffb.zugzwang.move.{Move, MoveList}

final class SearchStackEntry:
  var staticEval: Score    = Score.Zero
  var move: Move           = Move.None
  var piece: Piece         = Piece.NoPiece
  var capturedPiece: Piece = Piece.NoPiece
  var bestMove: Move       = Move.None
  var reduction: Depth     = Depth.Zero
  var inCheck: Boolean     = false
  var isPvNode: Boolean    = false

  var quietsTried: MoveList   = MoveList.initialize
  var capturesTried: MoveList = MoveList.initialize

  def addQuiet(move: Move): Unit   = quietsTried.add(move)
  def addCapture(move: Move): Unit = capturesTried.add(move)

  // TODO: revisit this. I'm not sure this works like I expect
  def isDefined: Boolean =
    !(staticEval == Score.Zero &&
      move.isNoMove &&
      piece.isNoPiece &&
      capturedPiece.isNoPiece &&
      bestMove.isNoMove &&
      reduction == Depth.Zero &&
      !inCheck &&
      !isPvNode)

  def clear(): Unit =
    staticEval = Score.Zero
    move = Move.None
    piece = Piece.NoPiece
    capturedPiece = Piece.NoPiece
    bestMove = Move.None
    reduction = Depth.Zero
    inCheck = false
    isPvNode = false

    quietsTried.clear()
    capturesTried.clear()

object SearchStackEntry:
  def Default: SearchStackEntry = new SearchStackEntry

final class SearchStack private (private val entries: Array[SearchStackEntry]):
  def at(ply: Ply): SearchStackEntry = entries(ply.toInt + SearchStack.Padding)

  def clear(): Unit =
    for i <- 0 until entries.size do entries(i).clear()

object SearchStack:
  private val Padding       = 4
  private val MaxSearchSize = 128 + Padding + 2

  def initialize(): SearchStack =
    new SearchStack(Array.fill(MaxSearchSize)(SearchStackEntry.Default))
