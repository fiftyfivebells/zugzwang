package com.ffb.zugzwang.rules

import com.ffb.zugzwang.chess.{
  CastleRights,
  CastleSide,
  Color,
  GameState,
  Piece,
  PieceType,
  Square
}
import com.ffb.zugzwang.move.{Move, MoveGenerator, MoveType}
import com.ffb.zugzwang.board.Board

object Rules:

  def pseudoLegalMoves(state: GameState): List[Move] =
    MoveGenerator.pseudoLegalMoves(state)

  def legalMoves(state: GameState): List[Move] =
    MoveGenerator.legalMoves(state)

  def applyMove(state: GameState, move: Move): GameState =
    val newBoard = Board.applyMove(state.board, move)

    val epSquare = state.board.pieceAt(move.from) match {
      case Some(piece) if piece.pieceType == PieceType.Pawn =>
        if move.moveType == MoveType.DoublePush then
          val epSq = state.activeSide match {
            case Color.White => Square(move.to.value - 8)
            case Color.Black => Square(move.to.value + 8)
          }

          Some(epSq)
        else None

      case _ => None
    }

    val movedPiece = state.board.pieceAt(move.from)
    val capturedPiece = state.board.pieceAt(move.to)

    val newCastleRights = (movedPiece, capturedPiece) match {
      case (Some(piece), _)
          if piece.pieceType == PieceType.King || piece.pieceType == PieceType.Rook =>
        updateCastleRights(
          state.board,
          state.castleRights,
          state.activeSide,
          move
        )
      case (_, Some(piece)) if piece.pieceType == PieceType.Rook =>
        updateCastleRights(
          state.board,
          state.castleRights,
          state.activeSide.enemy,
          move
        )

      case _ => state.castleRights
    }

    val newHalfMove =
      (state.board.pieceAt(move.from), state.board.pieceAt(move.to)) match {
        case (Some(piece), Some(_)) if piece.pieceType == PieceType.Pawn => 0
        case _ => state.halfMoveClock + 1
      }
    val newFullMove =
      if state.activeSide == Color.Black then state.fullMoveClock + 1
      else state.fullMoveClock

    GameState(
      board = newBoard,
      activeSide = state.activeSide.enemy,
      castleRights = newCastleRights,
      enPassant = epSquare,
      halfMoveClock = newHalfMove,
      fullMoveClock = newFullMove
    )

  def isValidMove(state: GameState, move: Move): Boolean =
    val newState = applyMove(state, move)
    isSideInCheck(newState, state.activeSide)

  def isSideInCheck(state: GameState, color: Color): Boolean =
    state.board.isKingAttacked(color)

  def isCheckmate(state: GameState): Boolean =
    legalMoves(state).isEmpty && isSideInCheck(state, state.activeSide)

  def isStaleMate(state: GameState): Boolean =
    legalMoves(state).isEmpty && !isSideInCheck(state, state.activeSide)

// TODO: there's more logic to implement here:
//       1. 50 move rule
//       2. insufficient matererial
  def isDraw(state: GameState): Boolean =
    isStaleMate(state)

  private def updateCastleRights(
      board: Board,
      rights: CastleRights,
      side: Color,
      move: Move
  ): CastleRights =
    val moved = board.pieceAt(move.from)
    val captured = board.pieceAt(move.to)

    (moved, captured) match {
      case (Some(piece), _) if piece.pieceType == PieceType.King =>
        rights.removeAll(side)

      case (Some(piece), _) if piece.pieceType == PieceType.Rook =>
        updateRookRights(side, move.from, rights)

      // looks similar, but this is updating the captured rook's player's rights
      case (_, Some(piece)) if piece.pieceType == PieceType.Rook =>
        updateRookRights(side, move.to, rights)

      case _ => rights
    }

  private def rookSquares(color: Color): (Square, Square) =
    if color == Color.White then (Square.H1, Square.A1)
    else (Square.H8, Square.A8)

  private def updateRookRights(
      color: Color,
      sq: Square,
      rights: CastleRights
  ): CastleRights =
    val (kingsideRook, queensideRook) = rookSquares(color)

    if sq == kingsideRook then rights.remove(color, CastleSide.Kingside)
    else if sq == queensideRook then rights.remove(color, CastleSide.Queenside)
    else rights

end Rules
