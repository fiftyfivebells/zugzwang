package com.ffb.zugzwang.evaluation

import com.ffb.zugzwang.chess.{GameState, MutablePosition, Square}
import com.ffb.zugzwang.move.{Move, MoveType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SEESpec extends AnyFlatSpec with Matchers:

  // Helper to create a position from FEN
  private def positionFromFen(fen: String): MutablePosition =
    MutablePosition.from(GameState.from(fen))

  // Helper to create a move from UCI notation
  private def moveFromUci(uci: String, fen: String): Move =
    val position = positionFromFen(fen)
    val from     = Square.fromAlgebraic(uci.substring(0, 2)).toOption.get
    val to       = Square.fromAlgebraic(uci.substring(2, 4)).toOption.get
    val piece    = position.pieceAt(to)

    val moveType = if piece.isNoPiece then MoveType.Quiet else MoveType.Capture
    Move(from, to, moveType)

  "SEE.see" should "return 0 for non-capture moves" in {
    val fen      = "8/8/8/8/4P3/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = Move(Square.E4, Square.E5, MoveType.Quiet)

    SEE.see(position, move) shouldBe 0
  }

  it should "return piece value for undefended captures" in {
    val fen      = "8/8/8/3n4/4P3/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    SEE.see(position, move) shouldBe 320 // Knight value
  }

  it should "correctly evaluate pawn takes pawn defended by pawn (equal trade)" in {
    // White pawn e4 captures black pawn d5, black pawn c6 defends d5
    val fen      = "8/8/2p5/3p4/4P3/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    SEE.see(position, move) shouldBe 0 // Pawn captures pawn (+100), black pawn recaptures (-100)
  }

  it should "correctly evaluate pawn takes queen defended by queen (winning)" in {
    // White pawn c5 captures black queen on d6, black queen on d8 defends d6
    val fen      = "3q4/8/3q4/2P5/8/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("c5d6", fen)

    // Pawn takes queen (+900), black queen recaptures (+100 cost), net +800
    SEE.see(position, move) shouldBe 800
  }

  it should "correctly evaluate queen takes pawn defended by pawn (losing)" in {
    // White queen takes black pawn on e5, black pawn on d6 defends e5
    val fen      = "8/8/3p4/4p3/8/8/8/4Q3 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e1e5", fen)

    // Queen takes pawn (+100), black pawn recaptures queen - losing for white
    // White wouldn't recapture, so SEE = 0 (clamped)
    SEE.see(position, move) shouldBe 0
  }

  it should "handle bishop takes knight defended by bishop (equal)" in {
    val fen      = "8/8/2b5/3n4/4B3/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    // Bishop takes knight (+320), bishop recaptures (-330)
    // Net is -10, but white doesn't have to take initially, so max(0, ...)
    SEE.see(position, move) shouldBe 0
  }

  it should "handle multiple defenders - rook captures defended by bishop and knight" in {
    // Rook takes pawn defended by knight and bishop
    val fen      = "8/8/2b5/8/2n1p3/4R3/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e3e4", fen)

    // Rook takes pawn (+100), knight recaptures (-500), then white wouldn't continue
    SEE.see(position, move) shouldBe 0
  }

  it should "handle discovered attacks - pawn blocks rook x-ray" in {
    val fen      = "8/8/8/3n4/4P3/8/4R3/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    // Pawn takes knight (+320), rook is revealed and defends
    // No black piece can recapture, so white wins knight
    SEE.see(position, move) shouldBe 320
  }

  it should "handle x-ray attacks through bishop" in {
    val fen      = "8/8/5b2/8/3n4/2B5/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("c3d4", fen)

    // Bishop takes knight (+320), bishop recaptures (-330)
    SEE.see(position, move) shouldBe 0
  }

  it should "handle king as attacker" in {
    val fen      = "8/8/8/8/3n4/4K3/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e3d4", fen)

    // King takes knight (+320), undefended
    SEE.see(position, move) shouldBe 320
  }

  it should "handle king as defender" in {
    val fen      = "8/8/8/3k4/3n4/4P3/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e3d4", fen)

    // Pawn takes knight (+320), king recaptures (-100)
    SEE.see(position, move) shouldBe 220
  }

  it should "handle en passant capture" in {
    val fen      = "8/8/8/3pP3/8/8/8/8 w - d6 0 1"
    val position = positionFromFen(fen)
    val epSquare = Square.fromAlgebraic("d6").toOption.get
    val move     = Move(Square.E5, epSquare, MoveType.EnPassant)

    SEE.see(position, move) shouldBe 100
  }

  it should "handle complex exchange - multiple pieces lined up" in {
    val fen      = "1k1r4/8/8/3n4/4P3/8/3R4/4K3 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    // Pawn takes knight (+320)
    // Rook could recapture (-100)
    // White rook recaptures (+500)
    // Black rook recaptures (-500)
    // Net: 320 - 100 + 500 - 500 = 220

    SEE.see(position, move) shouldBe 320
  }

  it should "handle promotion captures" in {
    val fen      = "4n3/4P3/8/8/8/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val from     = Square.fromAlgebraic("e7").toOption.get
    val to       = Square.fromAlgebraic("e8").toOption.get
    val move     = Move(from, to, MoveType.CapturePromotion)

    // Pawn takes knight and promotes to queen
    // Should be knight value (320) as the promotion happens after capture
    SEE.see(position, move) shouldBe 320
  }

  it should "recognize that least valuable attacker is chosen first" in {
    val fen      = "8/8/8/3n4/4PRQ1/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    // White has pawn, rook, and queen that can capture
    // Should use pawn first (least valuable)
    // Pawn takes knight (+320), undefended
    SEE.see(position, move) shouldBe 320
  }

  it should "handle captures near castling squares" in {
    // White rook a1 takes black rook a8. Black h8 rook is blocked by the king on e8.
    val fen      = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("a1a8", fen)

    // Black h8 rook cannot recapture (blocked by black king on e8), so white wins the rook
    SEE.see(position, move) shouldBe 500
  }

  it should "handle rook takes rook with open file (undefended)" in {
    // White rook d1 takes black rook d8, no other pieces on d-file to recapture
    val fen      = "3r4/8/8/8/8/8/8/3R4 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("d1d8", fen)

    SEE.see(position, move) shouldBe 500 // Black rook undefended, white wins it
  }

  it should "return 0 when initial capture exists but target is empty" in {
    val fen      = "8/8/8/8/8/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = Move(Square.E2, Square.E4, MoveType.Quiet)

    SEE.see(position, move) shouldBe 0
  }

  "SEE.seeGE" should "return true when SEE >= threshold" in {
    val fen      = "8/8/8/3n4/4P3/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    SEE.seeGE(position, move, 320) shouldBe true
    SEE.seeGE(position, move, 0) shouldBe true
    SEE.seeGE(position, move, 100) shouldBe true
  }

  it should "return false when SEE < threshold" in {
    // Equal trade: white pawn e4 takes black pawn d5, defended by black pawn c6 → SEE = 0
    val fen      = "8/8/2p5/3p4/4P3/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    // SEE is 0 (equal trade)
    SEE.seeGE(position, move, 1) shouldBe false
    SEE.seeGE(position, move, 100) shouldBe false
  }

  it should "handle negative thresholds correctly" in {
    // Losing capture: white queen takes black pawn on e5, defended by black pawn d6 → SEE = 0 (clamped)
    val fen      = "8/8/3p4/4p3/8/8/8/4Q3 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e1e5", fen)

    // SEE is 0 (losing capture, clamped to 0)
    SEE.seeGE(position, move, -100) shouldBe true
    SEE.seeGE(position, move, 0) shouldBe true
    SEE.seeGE(position, move, 1) shouldBe false
  }

  it should "work with more complex position for winning capture" in {
    // White pawn c5 takes black queen on d6, defended by black queen on d8 → SEE = 800
    val fen      = "3q4/8/3q4/2P5/8/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("c5d6", fen)

    // SEE is 800 (pawn takes queen 900, black queen recaptures 100 cost)
    SEE.seeGE(position, move, 800) shouldBe true
    SEE.seeGE(position, move, 700) shouldBe true
    SEE.seeGE(position, move, 900) shouldBe false
    SEE.seeGE(position, move, 0) shouldBe true
  }

  // ---------------------------------------------------------------------------
  // Rook x-ray / sliding piece x-ray tests
  // ---------------------------------------------------------------------------

  "SEE.see" should "handle rook x-ray attack revealed after capture" in {
    // White rook on e1, white pawn on e4, black knight on d5 (undefended)
    // Pawn takes knight: rook on e1 is NOT involved (pawn moves diagonally)
    // So this is a simple undefended capture = 320
    val fen      = "8/8/8/3n4/4P3/8/8/4R3 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    SEE.see(position, move) shouldBe 320
  }

  it should "handle rook x-ray: rook behind rook attacks same square" in {
    // Black rook on d8, white rook on d1 captures black rook on d5
    // After Rxd5, the black rook on d8 can recapture
    // Rook takes rook (+500), black rook recaptures (-500): equal trade
    val fen      = "3r4/8/8/3r4/8/8/8/3R4 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("d1d5", fen)

    SEE.see(position, move) shouldBe 0
  }

  it should "handle rook x-ray winning: white has two rooks, black has one" in {
    // White rooks on d1 and d2, black rook on d5 (undefended behind it on d8: none)
    // White Rd1 takes Rd5 (+500), black has nothing to recapture
    val fen      = "8/8/8/3r4/8/8/3R4/3R4 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("d1d5", fen)

    // Rook takes rook (+500), white rook d2 could recapture on d5
    // But black has no recapture, so white wins rook
    SEE.see(position, move) shouldBe 500
  }

  // ---------------------------------------------------------------------------
  // Bishop x-ray through another bishop
  // ---------------------------------------------------------------------------

  it should "handle bishop x-ray: bishop behind bishop on same diagonal" in {
    // White bishops on c3 and b2, black knight on d4
    // Bc3xd4 (+320), then white Bb2 is revealed on the c3-d4 diagonal
    // But no black piece on d4 diagonal can recapture
    val fen      = "8/8/8/8/3n4/2B5/1B6/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("c3d4", fen)

    // Bishop takes knight (+320), undefended by black
    SEE.see(position, move) shouldBe 320
  }

  // ---------------------------------------------------------------------------
  // Multiple pieces of same type defending
  // ---------------------------------------------------------------------------

  it should "handle two rooks defending: attacker should not capture" in {
    // White queen captures a pawn defended by two black rooks
    // Qxp (+100), Rxq (-900): losing, SEE = 0
    val fen      = "3r4/3r4/8/8/3p4/8/8/3Q4 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("d1d4", fen)

    SEE.see(position, move) shouldBe 0
  }

  it should "handle pawn takes pawn with three defenders (two pawns + knight)" in {
    // White pawn on e4 captures black pawn on d5
    // Black defends d5 with pawn (c6) and knight (f6)
    // White pawn takes pawn (+100), black pawn recaptures (-100): equal
    val fen      = "8/8/2p2n2/3p4/4P3/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    // Pawn captures pawn (+100), pawn on c6 recaptures (-100): SEE = 0
    SEE.see(position, move) shouldBe 0
  }

  // ---------------------------------------------------------------------------
  // Threshold boundary cases for seeGE
  // ---------------------------------------------------------------------------

  "SEE.seeGE" should "return true for threshold of 0 on any capture (SEE is always >= 0)" in {
    // Losing capture: white queen takes black pawn on e5, defended by black pawn d6 → SEE = 0 (clamped)
    val fen      = "8/8/3p4/4p3/8/8/8/4Q3 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e1e5", fen)

    SEE.seeGE(position, move, 0) shouldBe true
  }

  it should "return true for an equal trade at threshold 0" in {
    // White pawn e4 takes black pawn d5, defended by black pawn c6: equal trade, SEE = 0
    val fen      = "8/8/2p5/3p4/4P3/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e4d5", fen)

    SEE.seeGE(position, move, 0) shouldBe true
    SEE.seeGE(position, move, 1) shouldBe false
  }

  it should "correctly distinguish winning rook capture from losing one" in {
    // Pawn takes rook (undefended): SEE = 500
    val fenWin      = "8/8/8/3r4/4P3/8/8/8 w - - 0 1"
    val positionWin = positionFromFen(fenWin)
    val moveWin     = moveFromUci("e4d5", fenWin)

    SEE.seeGE(positionWin, moveWin, 500) shouldBe true
    SEE.seeGE(positionWin, moveWin, 501) shouldBe false

    // Equal trade: white rook d1 takes black rook d5, black rook d8 recaptures → SEE = 0
    val fenEq      = "3r4/8/8/3r4/8/8/8/3R4 w - - 0 1"
    val positionEq = positionFromFen(fenEq)
    val moveEq     = moveFromUci("d1d5", fenEq)

    SEE.seeGE(positionEq, moveEq, 0) shouldBe true
    SEE.seeGE(positionEq, moveEq, 1) shouldBe false
  }

  // ---------------------------------------------------------------------------
  // Non-standard capture scenarios
  // ---------------------------------------------------------------------------

  "SEE.see" should "return piece value for an undefended queen capture by knight" in {
    // Knight captures an undefended queen
    val fen      = "8/8/8/3q4/8/4N3/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("e3d5", fen)

    SEE.see(position, move) shouldBe 900 // Queen value
  }

  it should "return 180 for knight takes rook defended by knight" in {
    // White knight d4 takes black rook d5. Black knight f6 defends d5 (f6 attacks d5 and e4).
    // gain = [500 (rook), 320 (knight)].
    // resolve(1,0) = max(0, 320-0) = 320; resolve(0,320) = max(0, 500-320) = 180
    val fen      = "8/8/5n2/3r4/3N4/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("d4d5", fen)

    SEE.see(position, move) shouldBe 180
  }

  it should "handle capture on a1 corner (boundary square)" in {
    // Bishop on b2 takes undefended rook on a1
    val fen      = "8/8/8/8/8/8/1B6/r7 w - - 0 1"
    val position = positionFromFen(fen)
    val move     = moveFromUci("b2a1", fen)

    SEE.see(position, move) shouldBe 500 // Rook value, undefended
  }

  it should "return queen value when pawn promotes capturing queen" in {
    // Pawn on e7 captures queen on f8 and promotes
    // The captured piece is the queen (900), promotion happens after
    val fen      = "5q2/4P3/8/8/8/8/8/8 w - - 0 1"
    val position = positionFromFen(fen)
    val from     = Square.fromAlgebraic("e7").toOption.get
    val to       = Square.fromAlgebraic("f8").toOption.get
    val move     = Move(from, to, MoveType.CapturePromotion)

    // SEE sees pawn capturing queen (900)
    SEE.see(position, move) shouldBe 900
  }
