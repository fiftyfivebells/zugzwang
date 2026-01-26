package com.ffb.zugzwang.uci

import com.ffb.zugzwang.chess.{Color, GameState, MutablePosition}
import com.ffb.zugzwang.move.MoveGenerator
import com.ffb.zugzwang.notation.FENParser
import com.ffb.zugzwang.rules.Rules
import com.ffb.zugzwang.search.{Search, SearchLimits}
import com.ffb.zugzwang.tools.PerftRunner

import scala.annotation.tailrec
import scala.io.Source

object UciMain:

  def main(args: Array[String]): Unit =
    val inputLines = Source.stdin.getLines()
    uciLoop(GameState.initial, inputLines)

  @tailrec
  def uciLoop(state: GameState, lines: Iterator[String]): Unit =
    if lines.hasNext then
      val line   = lines.next()
      val tokens = line.split("\\s+").toList

      val nextState = tokens match
        case "uci" :: _ =>
          println("id name Zugzwang")
          println("id author Stephen Bell")
          println("uciok")
          state

        case "isready" :: _ =>
          println("readyok")
          state

        case "position" :: rest =>
          handlePosition(rest)

        case "print" :: _ =>
          println(s"\n${state.prettyPrint}")
          state

        case "go" :: rest =>
          val newState = handleGo(state, rest)
          newState

        case "quit" :: _ =>
          return

        case _ =>
          state // Ignore unknown and return current state

      uciLoop(nextState, lines)

  private def handlePosition(tokens: List[String]): GameState =
    val (baseState, remaining) = tokens match
      case "startpos" :: rest =>
        (GameState.initial, rest)

      case "fen" :: rest =>
        val fen      = rest.take(6).mkString(" ")
        val afterFen = rest.drop(6)

        val state = FENParser.from(fen) match
          case Right(state) =>
            state
          case Left(err) =>
            println("Error parsing FEN: ${err}")
            GameState.initial

        (state, afterFen)

      case _ =>
        (GameState.initial, tokens)

    remaining match
      case "moves" :: moveList => applyUciMoves(baseState, moveList)
      case _                   => baseState

  private def applyUciMoves(startState: GameState, moves: List[String]): GameState =
    moves.foldLeft(startState): (state, moveUci) =>
      val legalMoves = MoveGenerator.legalMoves(state)
      legalMoves.find(_.toUci == moveUci) match
        case Some(move) => Rules.applyMove(state, move)
        case None =>
          println(s"Ignoring illegal move: ${moveUci}")
          state

  private def handleGo(state: GameState, tokens: List[String]): GameState =
    tokens match
      case "perft" :: (d: String) :: _ =>
        val depth = d.toInt
        PerftRunner.runPerft(state, depth)
        println()
        state

      case _ =>
        val limits         = parseTime(tokens, state.activeSide)
        val searchPosition = MutablePosition.from(state)
        val bestMove       = Search.search(searchPosition, limits)

        println(s"bestmove ${bestMove.toUci}")
        state

  private def parseTime(params: List[String], side: Color): SearchLimits =
    // if movetime is there, it's a fixed time, so just return that
    findKeywordByValue(params, "movetime").map(_.toLong) match
      case Some(time) =>
        return SearchLimits(endTime = System.currentTimeMillis() + time)
      case None => // if movetime isn't there, just keep on rolling
    val wTime = findKeywordByValue(params, "wtime").map(_.toLong)
    val bTime = findKeywordByValue(params, "btime").map(_.toLong)

    val (timeOpt, incOpt) =
      side match
        case Color.White => (wTime, findKeywordByValue(params, "winc").map(_.toLong))
        case Color.Black => (bTime, findKeywordByValue(params, "binc").map(_.toLong))

    val myInc = incOpt.getOrElse(0L)

    timeOpt match
      case Some(time) =>
        val now = System.currentTimeMillis()

        val movesToGo = findKeywordByValue(params, "movestogo").map(_.toInt)

        val timeToSpend = movesToGo match
          case Some(moves) =>
            (time / moves) + myInc

          case None =>
            (time / 20) + (myInc / 2)

        val hardLimit = time - 50
        val safeTime  = Math.min(timeToSpend, hardLimit)

        val finalAllocation = Math.max(10, safeTime)

        SearchLimits(endTime = now + finalAllocation)

      case None =>
        val depth = findKeywordByValue(params, "depth").map(_.toInt).getOrElse(6)
        SearchLimits(depth = depth)

  private def findKeywordByValue(tokens: List[String], key: String): Option[String] =
    tokens.dropWhile(_ != key) match
      case `key` :: value :: _ => Some(value)
      case _                   => None
