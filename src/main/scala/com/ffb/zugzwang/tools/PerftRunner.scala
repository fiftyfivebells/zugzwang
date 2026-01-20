package com.ffb.zugzwang.tools
import com.ffb.zugzwang.move.Perft
import com.ffb.zugzwang.notation.FENParser

object PerftRunner:

  def main(args: Array[String]): Unit =
    val parsed =
      args
        .sliding(2, 2)
        .collect { case Array(key, value) => key -> value }
        .toMap

    val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    val fen   = parsed.getOrElse("--fen", initialFen)
    val depth = parsed.get("--depth").map(_.toInt).getOrElse(4)
    val mode  = parsed.getOrElse("--mode", "perft").toLowerCase

    println(s"FEN: $fen")
    println(s"Depth: $depth")
    println(s"Mode: $mode")

    FENParser.from(fen) match
      case Left(err) =>
        println(s"Invalid FEN: $err")

      case Right(state) =>
        val start = System.nanoTime()
        val nodesOpt =
          mode match
            case "divide" => Some(Perft.divide(state, depth))
            case "perft"  => Some(Perft.perftBasic(state, depth))
            case other =>
              println(s"Unknown mode: $other. Use perft or divide.")
              None

        nodesOpt.foreach { nodes =>
          val end = System.nanoTime()

          val seconds = (end - start) / 1e9
          val nps     = nodes / seconds

          println(s"Nodes: $nodes")
          println(f"Time: $seconds%.2f seconds")
          println(f"NPS: $nps%.0f")
        }

end PerftRunner
