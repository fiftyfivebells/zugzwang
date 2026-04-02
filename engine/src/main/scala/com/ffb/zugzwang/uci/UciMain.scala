package com.ffb.zugzwang.uci

import com.ffb.zugzwang.BuildInfo
import com.ffb.zugzwang.chess.{Color, GameState, MutablePosition}
import com.ffb.zugzwang.core.{Depth, SearchTime}
import com.ffb.zugzwang.move.{MoveGenerator, Perft}
import com.ffb.zugzwang.notation.FENParser
import com.ffb.zugzwang.rules.Rules
import com.ffb.zugzwang.search.{Search, SearchConfig, SearchLimits, SearchStats}
import com.ffb.zugzwang.tools.DebugLogger

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
          println("option name MoveOverhead type spin default 50 min 0 max 5000")
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

        case "setoption" :: "name" :: rest =>
          val parts = rest.mkString(" ").split("\\s+value\\s+", 2)
          if parts.length == 2 then
            val optName  = parts(0).trim
            val optValue = parts(1).trim
            if optName.toLowerCase == "moveoverhead" then
              try moveOverheadMs = optValue.toLong
              catch case _: NumberFormatException => ()
            else if !SearchConfig.setOption(optName, optValue) then DebugLogger.log(s"Unknown option: $optName")
          state

        case "position" :: rest =>
          handlePosition(rest)

        case "print" :: _ =>
          println(s"\n${state.prettyPrint}")
          println(s"\n\n${state.toFen}")
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
        val depth    = d.toInt
        val position = MutablePosition.from(state)
        val start    = System.nanoTime()
        val nodes    = Perft.divide(position, depth)
        val elapsed  = (System.nanoTime() - start) / 1e9
        val nps      = (nodes / elapsed).toLong
        println(f"Time:  $elapsed%.3f seconds")
        println(f"NPS:   $nps%,d")
        state

      case _ =>
        val limits         = parseTime(tokens, state.activeSide, state.fullMoveClock)
        val searchPosition = MutablePosition.from(state)

        isSearching = true
        searchThread = Thread { () =>
          try
            val bestMove = Search.search(searchPosition, limits)
            println(s"bestmove ${bestMove.toUci}")
            if isDebugMode then SearchStats.printReport()
          catch
            case e: Throwable =>
              System.err.println(s"FATAL: Search crashed: ${e.getClass().getName()}: ${e.getMessage()}")
              DebugLogger.log(s"FATAL: Search crashed: ${e.getClass().getName()}: ${e.toString()}")
              DebugLogger.log(e.getStackTrace().mkString("\n"))
          finally isSearching = false
        }
        searchThread.nn.start()

        state

  private var moveOverheadMs: Long = 50L

  private def parseTime(params: List[String], side: Color, moveNumber: Int): SearchLimits =
    def millisFrom(key: String): Option[Long] =
      findKeywordByValue(params, key).map(_.toLong)

    millisFrom("movetime") match
      case Some(time) =>
        val safeTime = math.max(1L, time - moveOverheadMs)
        SearchLimits(moveTime = SearchTime(safeTime), endTime = SearchTime(safeTime))

      case None =>
        val wTime = millisFrom("wtime")
        val bTime = millisFrom("btime")
        val wInc  = millisFrom("winc").getOrElse(0L)
        val bInc  = millisFrom("binc").getOrElse(0L)
        val mtg   = millisFrom("movestogo")

        val (timeOpt, inc) = side match
          case Color.White => (wTime, wInc)
          case Color.Black => (bTime, bInc)

        timeOpt match
          case Some(timeRemaining) =>
            val available = math.max(1L, timeRemaining - moveOverheadMs)
            val ply       = math.max(0, (moveNumber - 1) * 2) // approximate half-move count

            // Move horizon: ~50 moves normally, but shrink when clock is low.
            // At 500ms remaining, plan for ~2.5 moves instead of 50.
            val scaledTimeSec = available / 1000.0
            val centiMTG: Int = mtg match
              case Some(m) => math.min(m.toInt * 100, 5000)
              case None =>
                if available < 1000L then math.max(100, (available * 5.051 / 1000.0).toInt)
                else 5051

            // Total time budget across all remaining moves, accounting for
            // increment on future moves and overhead on every future move.
            val timeLeft = math.max(1L, available + (inc * (centiMTG - 100) - moveOverheadMs * (200 + centiMTG)) / 100)

            val (optScale, maxScale): (Double, Double) = mtg match
              case None =>
                // Sudden death: log-scaled constants (Stockfish model)
                val logTimeSec  = math.log10(math.max(0.001, scaledTimeSec))
                val optConstant = math.min(0.0029869 + 0.00033554 * logTimeSec, 0.004905)
                val maxConstant = math.max(3.3744 + 3.0608 * logTimeSec, 3.1441)
                val opt = math.min(
                  0.012112 + math.pow(ply + 3.23, 0.469) * optConstant,
                  0.194 * available.toDouble / timeLeft
                )
                val mx = math.min(6.873, maxConstant + ply / 12.35)
                (opt, mx)
              case Some(_) =>
                // Moves-to-go mode
                val mtgMoves = centiMTG / 100.0
                val opt = math.min(
                  (0.88 + ply / 116.4) / mtgMoves,
                  0.88 * available.toDouble / timeLeft
                )
                val mx = math.min(6.873, 1.3 + 0.11 * mtgMoves)
                (opt, mx)

            val softBudget = math.max(1L, (optScale * timeLeft).toLong)
            val hardBudget = math
              .max(
                softBudget,
                math.min(
                  (0.81 * available - moveOverheadMs).toLong,
                  (maxScale * softBudget).toLong
                )
              )
              .max(2L)

            SearchLimits(
              moveTime = SearchTime(softBudget),
              endTime = SearchTime(hardBudget),
              ply = ply
            )

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
