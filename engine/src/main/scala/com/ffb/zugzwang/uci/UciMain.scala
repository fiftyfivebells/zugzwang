package com.ffb.zugzwang.uci

import com.ffb.zugzwang.chess.{GameState, MutablePosition}
import com.ffb.zugzwang.move.MoveGenerator
import com.ffb.zugzwang.notation.FENParser
import com.ffb.zugzwang.rules.Rules
import com.ffb.zugzwang.search.Search
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
        val depth          = findKeywordByValue(tokens, "depth").map(_.toInt).getOrElse(5)
        val searchPosition = MutablePosition.from(state)
        val bestMove       = Search.findBestMove(searchPosition, depth)

        println(s"bestmove ${bestMove.toUci}")

        state

  private def findKeywordByValue(tokens: List[String], key: String): Option[String] =
    tokens.dropWhile(_ != key) match
      case `key` :: value :: _ => Some(value)
      case _                   => None
