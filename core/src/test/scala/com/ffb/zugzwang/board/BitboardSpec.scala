package com.ffb.zugzwang.board

import com.ffb.zugzwang.chess.Square
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BitboardSpec extends AnyFlatSpec with Matchers:

  "Bitboard.fromSquare" should "produce a board with exactly one bit set" in {
    Square.from(0) map { sq =>
      val bb = Bitboard.from(sq)
      bb.toLong shouldEqual 1L
    }

    Square.from(63) map { sq =>
      val bb = Bitboard.from(sq)
      bb.toLong shouldEqual (1L << 63)
    }
  }

  it should "be empty when constructed from no squares" in {
    Bitboard.empty.isEmpty shouldBe true
    Bitboard.empty.popCount shouldEqual 0
  }

  "Bitboard.add and remove" should "toggle individual bits correctly" in {
    val squares = List(Square.from(0), Square.from(7), Square.from(63)).flatten

    squares foreach { sq =>
      val b      = Bitboard.empty
      val setBit = b.setBitAt(sq)

      setBit.isEmpty shouldBe false
      setBit.popCount shouldEqual 1
      setBit.leastSignificantBit shouldBe Some(sq)

      val clearedBit = setBit.clearBitAt(sq)

      clearedBit.isEmpty shouldBe true
      clearedBit.popCount shouldEqual 0
      clearedBit.leastSignificantBit shouldBe None
    }
  }

  "Bitboard.|" should "combine two bitboards" in {
    Square.from(0) flatMap { sq1 =>
      Square.from(1) map { sq2 =>
        val or = Bitboard.from(sq1) | Bitboard.from(sq2)
        or.popCount shouldEqual 2
        or.value shouldEqual 3L
      }
    }
  }

  "Bitboard.clearLsb" should "remove only the least significant bit" in {
    Square.from(0) flatMap { sq1 =>
      Square.from(1) map { sq2 =>
        val bb = Bitboard.from(sq1, sq2)

        bb.leastSignificantBit shouldEqual Some(0)
        val cleared = bb.removeLsb

        cleared.value shouldEqual 2L
      }
    }
  }
