package com.ffb.zugzwang.chess

import com.ffb.zugzwang.board.Board

final case class CastleRights(
    kingsideWhite: Boolean,
    queensideWhite: Boolean,
    kingsideBlack: Boolean,
    queensideBlack: Boolean
)

object CastleRights:
  val initial: CastleRights =
    CastleRights(
      kingsideWhite = true,
      queensideWhite = true,
      kingsideBlack = true,
      queensideBlack = true
    )

  def updateRights(
      c: Color,
      rights: CastleRights,
      kingside: Boolean = true,
      queenside: Boolean = true
  ): CastleRights = c match {
    case Color.White =>
      rights.copy(kingsideWhite = kingside, queensideWhite = queenside)
    case Color.Black =>
      rights.copy(kingsideBlack = kingside, queensideBlack = queenside)
  }

end CastleRights

final case class GameState(
    board: Board,
    activeSide: Color,
    castleRights: CastleRights,
    enPassant: Option[Square],
    halfMoveClock: Int,
    fullMoveClock: Int
):
  def hasCastleRights: Boolean = activeSide match {
    case Color.White =>
      castleRights.kingsideWhite || castleRights.queensideWhite
    case Color.Black =>
      castleRights.kingsideBlack || castleRights.queensideBlack
  }

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

end GameState
