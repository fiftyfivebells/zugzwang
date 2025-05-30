package com.ffb.zugzwang.benchmarks

import com.ffb.zugzwang.*
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
class MoveGenBenchmark {

  @Setup(Level.Trial)
  def setup(): Unit = {
    // initialize a mid‑game position
    // zug = ZugzwangEngine.fromFEN(
    //   "rnbq1rk1/ppp2ppp/4pn2/3p4/3P4/2N1PN2/PPPB1PPP/R2QKB1R w KQ - 0 1"
    // )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(value = java.util.concurrent.TimeUnit.SECONDS)
  def perft4(): Long = {
    val fen = "rnbq1rk1/ppp2ppp/4pn2/3p4/3P4/2N1PN2/PPPB1PPP/R2QKB1R w KQ - 0 1"
    Zugzwang.perft(fen, 4)
  }
}
