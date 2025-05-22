package com.ffb.zugzwang.move

import com.ffb.zugzwang.chess.{
  CastleSide,
  Color,
  GameState,
  Piece,
  PieceType,
  Square
}
import com.ffb.zugzwang.board.{Board, Bitboard, PieceCategory}
import com.ffb.zugzwang.rules.Rules
import com.ffb.zugzwang.chess.CastleRights

object MoveGenerator:
  def legalMoves(state: GameState): List[Move] =
    pseudoLegalMoves(state) filter { move =>
      val newState = Rules.applyMove(state, move)
      !Rules.isSideInCheck(newState, state.activeSide)
    }

  def pseudoLegalMoves(state: GameState): List[Move] =
    val moves = MoveList(256)

    val targets = state.board.byColor(state.activeSide.enemy)

    // this is the bulk of the moves. This should cover all of the attacks for every piece,
    // and then quiet moves for all pieces except for pawns
    PieceCategory.byColor(state.activeSide) foreach { pc =>
      // handle pawns separately
      val pieces = state.board.pieces(pc.ordinal)
      val piece = Piece.from(pc)

      pieces.squares.toList foreach { from =>
        val rawAttacks = Attacks.attacks(piece, from, state.board.occupied)
        val attackMask = rawAttacks & targets
        val quietMask = rawAttacks & ~state.board.occupied

        if pc == PieceCategory.WP || pc == PieceCategory.BP then
          val pawnAttacks = rawAttacks & (attackMask | (state.enPassant match {
            case Some(sq) => 1L << sq.value
            case None     => 0L
          }))
          // first generate pawn attacks
          pawnAttacks.squares.foreach { to =>
            if state.enPassant.isDefined && to == state.enPassant.get then
              state.enPassant.foreach(sq =>
                moves.add(Move(from, to, None, MoveType.EnPassant))
              )
            else if to.lastRank(state.activeSide) then
              addPromotions(from, to, isCapture = true, moves)
            else moves.add(Move(from, to, None, MoveType.Capture))
          }

          // TODO: can i make this faster by breaking this out and doing all the pawns in one shot?
          // now handle pawn pushes
          val singlePush =
            ~state.board.occupied & (state.activeSide match {
              case Color.White => (1L << from.value) << 8
              case Color.Black => (1L << from.value) >>> 8
            })
          val doublePush =
            ~state.board.occupied & (state.activeSide match {
              case Color.White => singlePush << 8
              case Color.Black => singlePush >>> 8
            }) // (singlePush << direction)

          singlePush.squares.foreach { to =>
            if to.lastRank(state.activeSide) then
              addPromotions(from, to, isCapture = false, moves)
            else moves.add(Move(from, to, None, MoveType.Quiet))
          }

          if from.startingPawnRank(state.activeSide) then
            doublePush.squares.foreach { to =>
              moves.add(Move(from, to, None, MoveType.DoublePush))
            }
        else
          attackMask.squares.foreach { to =>
            moves.add(Move(from, to, None, MoveType.Capture))
          }

          quietMask.squares.foreach { to =>
            moves.add(Move(from, to, None, MoveType.Quiet))
          }
      }
    }

    // castles
    if state.hasCastleRights then
      castleMoves(
        state.board.occupied,
        state.activeSide,
        state.castleRights,
        state.board,
        moves
      )

    moves.toList

  private def castleMoves(
      occupied: Bitboard,
      activeSide: Color,
      castleRights: CastleRights,
      board: Board,
      moves: MoveList
  ): Unit =
    if castleRights.has(activeSide, CastleSide.Kingside)
    then kingsideCastles(occupied, activeSide, board, moves)
    if castleRights.has(activeSide, CastleSide.Queenside)
    then queensideCastles(occupied, activeSide, board, moves)

  private def kingsideCastles(
      occupied: Bitboard,
      activeSide: Color,
      board: Board,
      moves: MoveList
  ): Unit =
    val (squares, mask) =
      if activeSide == Color.White then
        (List(Square.F1, Square.G1), Bitboard.f1g1Mask)
      else (List(Square.F8, Square.G8), Bitboard.f8g8mask)

    val (from, to) =
      if activeSide == Color.White then (Square.E1, Square.G1)
      else (Square.E8, Square.G8)

    if squares.forall(
        !board.isAttacked(_, activeSide)
      ) && (occupied & mask).isEmpty && !board.isKingAttacked(activeSide)
    then moves.add(Move(from, to, None, MoveType.CastleKingside))

  private def queensideCastles(
      occupied: Bitboard,
      activeSide: Color,
      board: Board,
      moves: MoveList
  ): Unit =
    val (squares, mask) =
      if activeSide == Color.White then
        (List(Square.C1, Square.D1), Bitboard.b1c1d1Mask)
      else (List(Square.C8, Square.D8), Bitboard.b8c8d8Mask)

    val (from, to) =
      if activeSide == Color.White then (Square.E1, Square.C1)
      else (Square.E8, Square.C8)

    if squares.forall(
        !board.isAttacked(_, activeSide)
      ) && (occupied & mask).isEmpty && !board.isKingAttacked(activeSide)
    then moves.add(Move(from, to, None, MoveType.CastleQueenside))

  private def addPromotions(
      from: Square,
      to: Square,
      isCapture: Boolean,
      moves: MoveList
  ): Unit =
    val moveType =
      if isCapture then MoveType.CapturePromotion else MoveType.Promotion

    val promotions =
      List(PieceType.Knight, PieceType.Bishop, PieceType.Rook, PieceType.Queen)

    promotions foreach { pt => moves.add(Move(from, to, Some(pt), moveType)) }
