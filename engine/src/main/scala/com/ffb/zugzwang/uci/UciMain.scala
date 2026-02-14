package com.ffb.zugzwang.uci

import com.ffb.zugzwang.BuildInfo
import com.ffb.zugzwang.chess.{Color, GameState, MutablePosition}
import com.ffb.zugzwang.core.{Depth, SearchTime}
import com.ffb.zugzwang.move.MoveGenerator
import com.ffb.zugzwang.notation.FENParser
import com.ffb.zugzwang.rules.Rules
import com.ffb.zugzwang.search.{Search, SearchLimits, SearchStats}
import com.ffb.zugzwang.tools.PerftRunner

import scala.annotation.tailrec
import scala.io.Source

object UciMain:
  private var isDebugMode = false
  @volatile
  private var searchThread: Thread | Null = null
  @volatile
  private var isSearching: Boolean = false

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
          val versionStr =
            if BuildInfo.gitCommit.nonEmpty then s"${BuildInfo.version}-dev.${BuildInfo.gitCommit}"
            else BuildInfo.version

          println(s"id name Zugzwang $versionStr")
          println(s"id author Stephen Bell")
          println("uciok")
          state

        case "ucinewgame" :: _ =>
          if isSearching then state
          else
            Search.clear()
            GameState.initial

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

        case "debug" :: rest =>
          handleDebug(rest)
          state

        case "stop" :: _ =>
          if isSearching then Search.requestStop()
          state

        case "quit" :: _ =>
          Search.requestStop()
          if searchThread != null then searchThread.nn.join(1000)
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
            println(s"Error parsing FEN: ${err}")
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
    SearchStats.reset()
    tokens match
      case "perft" :: (d: String) :: _ =>
        val depth = d.toInt
        PerftRunner.runPerft(state, depth)
        println()
        state

      case _ =>
        val limits         = parseTime(tokens, state.activeSide)
        val searchPosition = MutablePosition.from(state)

        isSearching = true
        searchThread = Thread { () =>
          try
            val bestMove = Search.search(searchPosition, limits)
            println(s"bestmove ${bestMove.toUci}")
            if isDebugMode then SearchStats.printReport()
          finally isSearching = false
        }
        searchThread.nn.start()

        state

  private def parseTime(params: List[String], side: Color): SearchLimits =
    def millisFrom(key: String): Option[Long] =
      findKeywordByValue(params, key).map(_.toLong)

    millisFrom("movetime") match
      case Some(time) =>
        // Fixed time search
        SearchLimits(moveTime = SearchTime(time))

      case None =>
        val wTime = millisFrom("wtime")
        val bTime = millisFrom("btime")
        val wInc  = millisFrom("winc").getOrElse(0L)
        val bInc  = millisFrom("binc").getOrElse(0L)

        val (timeOpt, inc) = side match
          case Color.White => (wTime, wInc)
          case Color.Black => (bTime, bInc)

        timeOpt match
          case Some(timeRemaining) =>
            val estimatedMovesToGo = 30

            val baseTime = timeRemaining / estimatedMovesToGo
            val budget   = baseTime + inc

            val maxBudget    = timeRemaining / 7
            val cappedBudget = math.min(budget, maxBudget)

            val safeBudget = math.max(10, cappedBudget - 20)

            SearchLimits(moveTime = SearchTime(safeBudget))

          case None =>
            val depth = findKeywordByValue(params, "depth").map(_.toInt).getOrElse(6)
            SearchLimits(depth = Depth(depth))

  private def findKeywordByValue(tokens: List[String], key: String): Option[String] =
    tokens.dropWhile(_ != key) match
      case `key` :: value :: _ => Some(value)
      case _                   => None

  private def handleDebug(params: List[String]): Unit =
    params.headOption match
      case Some(param) if param == "on"  => isDebugMode = true
      case Some(param) if param == "off" => isDebugMode = false
      case _                             => ()
