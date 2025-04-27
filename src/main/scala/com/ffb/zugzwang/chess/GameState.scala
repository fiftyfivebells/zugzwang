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
end CastleRights

final case class GameState(
    board: Board,
    activeSide: Color,
    castleRights: CastleRights,
    enPassant: Option[Square],
    halfMoveClock: Int,
    fullMoveClock: Int
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

end GameState
