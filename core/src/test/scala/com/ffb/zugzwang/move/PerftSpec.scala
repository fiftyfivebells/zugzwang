package com.ffb.zugzwang.move

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.ffb.zugzwang.chess.{Color, GameState, Square}
import com.ffb.zugzwang.rules.Rules
import com.ffb.zugzwang.board.{Bitboard, Board}

class PerftSpec extends AnyFlatSpec with Matchers:
  "perft" should "return correct number of moves for different depths from initial position" in {
    val state = GameState.initial

    Perft.perft(state, 1) shouldBe 20
    Perft.perft(state, 2) shouldBe 400
    Perft.perft(state, 3) shouldBe 8902
    Perft.perft(state, 4) shouldBe 197281
  }

  "perft" should "return correct number of moves for different depths from kiwipete position" in {

    val state = GameState.from(
      "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    )

    Perft.perft(state, 1) shouldBe 48
    Perft.perft(state, 2) shouldBe 2039
    Perft.perft(state, 3) shouldBe 97862
    Perft.perft(state, 4) shouldBe 4085603
  }
