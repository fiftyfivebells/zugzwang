package com.ffb.zugzwang.tools

import com.ffb.zugzwang.move.Perft
import com.ffb.zugzwang.notation.FENParser

object PerftRunner:

  val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  def main(args: Array[String]): Unit =
    val parsed = args
      .sliding(2, 2)
      .collect { case Array(key, value) => key -> value }
      .toMap

    val fen   = parsed.getOrElse("--fen", initialFen)
    val depth = parsed.get("--depth").map(_.toInt).getOrElse(4)
    val mode  = parsed.getOrElse("--mode", "perft").toLowerCase

    FENParser.from(fen) match
      case Left(err) =>
        println(s"Invalid FEN: $err")
        sys.exit(1)

      case Right(state) =>
        println(s"FEN:   $fen")
        println(s"Depth: $depth")
        println(s"Mode:  $mode")
        println()

        val start = System.nanoTime()

        val nodes = mode match
          case "divide" => Perft.divide(state, depth)
          case "perft"  => Perft.perft(state, depth)
          case other =>
            println(s"Unknown mode '$other'. Use: perft | divide")
            sys.exit(1)

        val elapsed = (System.nanoTime() - start) / 1e9
        val nps     = (nodes / elapsed).toLong

        println(s"Nodes: $nodes")
        println(f"Time:  $elapsed%.3f seconds")
        println(f"NPS:   $nps%,d")
