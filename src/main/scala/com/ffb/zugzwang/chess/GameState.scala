package com.ffb.zugzwang.chess

import com.ffb.zugzwang.board.Board
import scala.collection.mutable

enum CastleSide:
  case Kingside, Queenside

opaque type CastleRights = Int

object CastleRights:

  val whiteKingside = 1 << 0 // 0b0001
  val whiteQueenside = 1 << 1 // 0b0010
  val blackKingside = 1 << 2 // 0b0100
  val blackQueenside = 1 << 3 // 0b1000

  private def apply(flags: Int): CastleRights = flags

  val initial: CastleRights = from("KQkq")

  def from(fen: String): CastleRights =
    var rights: Int = 0

    if fen.contains("K") then rights |= whiteKingside
    if fen.contains("Q") then rights |= whiteQueenside
    if fen.contains("k") then rights |= blackKingside
    if fen.contains("q") then rights |= blackQueenside

    CastleRights(rights)

  extension (cr: CastleRights)
    def has(color: Color, side: CastleSide): Boolean =
      (cr & mask(color, side)) != 0

    def add(color: Color, side: CastleSide): CastleRights =
      CastleRights(cr | mask(color, side))

    def remove(color: Color, side: CastleSide): CastleRights =
      CastleRights(cr & ~mask(color, side))

    def removeAll(color: Color): CastleRights =
      val colorMask =
        mask(color, CastleSide.Kingside) | mask(color, CastleSide.Queenside)

      CastleRights(cr & ~colorMask)

    def isEmpty: Boolean = cr == 0

    def toFen: String =
      val sb = new mutable.StringBuilder

      if cr.hasFlag(whiteKingside) then sb.append("K")
      if cr.hasFlag(whiteQueenside) then sb.append("Q")
      if cr.hasFlag(blackKingside) then sb.append("k")
      if cr.hasFlag(blackQueenside) then sb.append("q")

      sb.toString

    private def hasFlag(flag: Int): Boolean = (cr & flag) != 0

    private def mask(color: Color, side: CastleSide): Int =
      (color, side) match
        case (Color.White, CastleSide.Kingside)  => whiteKingside
        case (Color.White, CastleSide.Queenside) => whiteQueenside
        case (Color.Black, CastleSide.Kingside)  => blackKingside
        case (Color.Black, CastleSide.Queenside) => blackQueenside

  end extension

end CastleRights

final case class GameState(
    board: Board,
    activeSide: Color,
    castleRights: CastleRights,
    enPassant: Option[Square],
    halfMoveClock: Int,
    fullMoveClock: Int
):
  def hasCastleRights: Boolean =
    castleRights.has(activeSide, CastleSide.Kingside) || castleRights.has(
      activeSide,
      CastleSide.Queenside
    )

object GameState:
  val initial: GameState =
    GameState(
      board = Board.initial,
      activeSide = Color.White,
      castleRights = CastleRights.initial,
      enPassant = None,
      halfMoveClock = 0,
      fullMoveClock = 1
    )

  def from(fen: String): GameState =
    val Array(
      boardFen,
      side,
      rights,
      ep,
      halfMove,
      fullMove
    ) = fen.split("\\s+")

    val board = Board.from(boardFen)
    val activeSide = if side == "w" then Color.White else Color.Black
    val castleRights = CastleRights.from(rights)
    val enPassant = Square.fromAlgebraic(ep).toOption

    // TODO: with well-validated fen strings, it should never be possible for these values to
    // fail to convert to ints. eventually, i want to add a fen parsing object that has a built
    // in fen validation, but in the meantime, i'll use this to prevent a catastrophe
    val halfMoveClock = halfMove.toIntOption.getOrElse(-1)
    val fullMoveClock = fullMove.toIntOption.getOrElse(-1)

    GameState(
      board,
      activeSide,
      castleRights,
      enPassant,
      halfMoveClock,
      fullMoveClock
    )

end GameState
