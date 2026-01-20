package com.ffb.zugzwang.rules

import com.ffb.zugzwang.board.Board
import com.ffb.zugzwang.chess.{CastleRights, CastleSide, Color, GameState, Piece, PieceType, Square}
import com.ffb.zugzwang.move.{Move, MoveGenerator, MoveType}

object Rules:

  def pseudoLegalMoves(state: GameState): List[Move] =
    MoveGenerator.pseudoLegalMoves(state)

  def legalMoves(state: GameState): List[Move] =
    MoveGenerator.legalMoves(state)

  def applyMove(state: GameState, move: Move): GameState =
    val newBoard = Board.applyMove(state.board, move)

    val epSquare =
      val fromPiece = state.board.pieceAt(move.from)
      if fromPiece.isPawn && move.moveType == MoveType.DoublePush then
        val epSq = state.activeSide match
          case Color.White => Square(move.to.value - 8)
          case Color.Black => Square(move.to.value + 8)
        Some(epSq)
      else None

    val movedPiece    = state.board.pieceAt(move.from)
    val capturedPiece = state.board.pieceAt(move.to)

    val newCastleRights =
      if movedPiece.isKing || movedPiece.isRook then
        updateCastleRights(
          state.board,
          state.castleRights,
          state.activeSide,
          move
        )
      else if capturedPiece.isRook then
        updateCastleRights(
          state.board,
          state.castleRights,
          state.activeSide.enemy,
          move
        )
      else state.castleRights

    val newHalfMove =
      val fromPiece = state.board.pieceAt(move.from)
      val toPiece   = state.board.pieceAt(move.to)

      if fromPiece.isPawn || Move.isCapture(move) then 0
      else state.halfMoveClock + 1

    val newFullMove =
      if state.activeSide == Color.Black then state.fullMoveClock + 1
      else state.fullMoveClock

    GameState(
      board = newBoard,
      activeSide = state.activeSide.enemy,
      castleRights = newCastleRights,
      enPassant = epSquare,
      halfMoveClock = newHalfMove,
      fullMoveClock = newFullMove,
      history = state.toFen :: state.history
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

  // TODO: this is pretty ugly. it looks like it works for now, but it could perhaps
  // use some cleaning up in the future
  def isInsufficientMaterial(state: GameState): Boolean =
    val pieces  = state.board.allPieces
    val grouped = pieces.groupMapReduce(_.pieceType)(_ => 1)(_ + _)

    val total = grouped.values.sum

    val bothKings = grouped.getOrElse(PieceType.King, 0) == 2

    // only two pieces and they're both kings
    if total == 2 && bothKings then true

    // 3 pieces: two are kings, and the other is a bishop or knight
    else if total == 3 && bothKings &&
      (
        grouped.get(PieceType.Knight).contains(1) ||
          grouped.get(PieceType.Bishop).contains(1)
      )
    then true

    // 4 pieces: KB vs kb, bishops on same colored squares
    else if total == 4 && bothKings && grouped.get(PieceType.Bishop).contains(2)
    then
      // helper to determine the color of a square: 0 is light, 1 is dark
      def squareColor(sq: Square): Int = (sq.file.value + sq.rank.value) % 2

      // TODO: figure out how to avoid the tuple here
      val bishopsByColor = state.board.squares.zipWithIndex.collect {
        case (piece, i) if piece.isBishop =>
          (piece.color, squareColor(Square(i)))
      }

      val whites = bishopsByColor.collect { case (Color.White, c) => c }
      val blacks = bishopsByColor.collect { case (Color.Black, c) => c }

      whites.size == 1 && blacks.size == 1 && whites.head == blacks.head
    else false

// TODO: there's more logic to implement here:
//       1. 50 move rule
//       2. insufficient matererial
  def isDraw(state: GameState): Boolean =
    isStaleMate(state) || isInsufficientMaterial(state) || state.halfMoveClock == 100

  private def updateCastleRights(
    board: Board,
    rights: CastleRights,
    side: Color,
    move: Move
  ): CastleRights =
    val moved    = board.pieceAt(move.from)
    val captured = board.pieceAt(move.to)

    if moved.isKing then rights.removeAll(side)
    else if moved.isRook then updateRookRights(side, move.from, rights)
    else if captured.isRook then updateRookRights(side, move.to, rights)
    else rights

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
