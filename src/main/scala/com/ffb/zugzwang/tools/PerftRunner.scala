package com.ffb.zugzwang.tools

import com.ffb.zugzwang.chess.GameState
import com.ffb.zugzwang.notation.FENParser
import com.ffb.zugzwang.move.Perft

object PerftRunner:

  def main(args: Array[String]): Unit =
    val parsed =
      args
        .sliding(2, 2)
        .toList
        .map(pair => pair.head -> pair.last)
        .toMap

    val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    val fen = parsed.getOrElse("--fen", initialFen)
    val depth = parsed.get("--depth").map(_.toInt).getOrElse(4)

    println(s"FEN: $fen")
    println(s"Depth: $depth")

    FENParser.from(fen) match {
      case Left(err) =>
        println(s"Invalid FEN: $err")

      case Right(state) =>
        val start = System.nanoTime()
        val nodes = Perft.perft(state, depth)
        val end = System.nanoTime()

        val seconds = (end - start) / 1e9
        val nps = nodes / seconds

        println(s"Nodes: $nodes")
        println(f"Time: $seconds%.2f seconds")
        println(f"NPS: $nps%.0f")
    }

end PerftRunner
