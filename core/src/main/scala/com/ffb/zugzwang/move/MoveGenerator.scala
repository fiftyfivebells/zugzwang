package com.ffb.zugzwang.move

import com.ffb.zugzwang.board.{Bitboard, Board}
import com.ffb.zugzwang.chess.{CastleRights, CastleSide, Color, GameState, MutablePosition, Piece, PieceType, Square}
import com.ffb.zugzwang.rules.Rules

object MoveGenerator:
  def legalMoves(state: GameState): List[Move] =
    pseudoLegalMoves(state) filter { move =>
      val newState = Rules.applyMove(state, move)
      !Rules.isSideInCheck(newState, state.activeSide)
    }

  def pseudoLegalMovesMutable(position: MutablePosition): List[Move] =
    val moves = MoveList(256)

    val occupied     = position.occupied
    val activePieces = position.byColor(position.activeSide.ordinal)
    val targets      = position.byColor(position.activeSide.enemy.ordinal)
    // this is the bulk of the moves. This should cover all of the attacks for every piece,
    // and then quiet moves for all pieces except for pawns
    Piece.byColor(position.activeSide) foreach { piece =>
      // handle pawns separately
      var pieces = position.pieces(piece)

      while pieces.nonEmpty do
        val from = Square(pieces.trailingZeros)
        pieces = pieces.removeLsb

        val rawAttacks = Attacks.attacks(piece, from, occupied)
        var attackMask = rawAttacks & targets
        var quietMask  = rawAttacks & ~occupied

        if piece == Piece.WhitePawn || piece == Piece.BlackPawn then
          var pawnAttacks = rawAttacks & (attackMask | (position.enPassantSq match
            case Some(sq) => 1L << sq.value
            case None     => 0L
          ))
          // first generate pawn attacks
          while pawnAttacks.nonEmpty do
            val to = Square(pawnAttacks.trailingZeros)
            pawnAttacks = pawnAttacks.removeLsb

            if position.enPassantSq.isDefined && to == position.enPassantSq.get then
              position.enPassantSq.foreach(sq => moves.add(Move(from, to, MoveType.EnPassant)))
            else if to.lastRank(position.activeSide) then addPromotions(from, to, isCapture = true, moves)
            else moves.add(Move(from, to, MoveType.Capture))

          // TODO: can i make this faster by breaking this out and doing all the pawns in one shot?
          // now handle pawn pushes
          var singlePush =
            ~occupied & (position.activeSide match
              case Color.White => (1L << from.value) << 8
              case Color.Black => (1L << from.value) >>> 8
            )
          var doublePush =
            ~occupied & (position.activeSide match
              case Color.White => singlePush << 8
              case Color.Black => singlePush >>> 8
            ) // (singlePush << direction)

          while singlePush.nonEmpty do
            val to = Square(singlePush.trailingZeros)
            singlePush = singlePush.removeLsb

            if to.lastRank(position.activeSide) then addPromotions(from, to, isCapture = false, moves)
            else moves.add(Move(from, to, MoveType.Quiet))

          if from.startingPawnRank(position.activeSide) then
            while doublePush.nonEmpty do
              val to = Square(doublePush.trailingZeros)
              doublePush = doublePush.removeLsb

              moves.add(Move(from, to, MoveType.DoublePush))
        else
          while attackMask.nonEmpty do
            val to = Square(attackMask.trailingZeros)
            attackMask = attackMask.removeLsb

            moves.add(Move(from, to, MoveType.Capture))

          while quietMask.nonEmpty do
            val to = Square(quietMask.trailingZeros)
            quietMask = quietMask.removeLsb

            moves.add(Move(from, to, MoveType.Quiet))
    }

    // castles
    if !position.castleRights.isEmpty then
      castleMovesMutable(
        occupied,
        position.activeSide,
        position.castleRights,
        position,
        moves
      )

    moves.toList

  def pseudoLegalCapturesMutable(position: MutablePosition): List[Move] =
    val moves = MoveList(256)

    val occupied = position.occupied
    val targets  = position.byColor(position.activeSide.enemy.ordinal)

    Piece.byColor(position.activeSide) foreach { piece =>
      var pieces = position.pieces(piece)

      while pieces.nonEmpty do
        val from = Square(pieces.trailingZeros)
        pieces = pieces.removeLsb

        val rawAttacks = Attacks.attacks(piece, from, occupied)
        var attackMask = rawAttacks & targets

        if piece.isPawn then
          val epMask = position.enPassantSq match
            case Some(sq) => 1L << sq.value
            case None     => 0L

          var pawnAttacks = attackMask | (rawAttacks & epMask)

          while pawnAttacks.nonEmpty do
            val to = Square(pawnAttacks.trailingZeros)
            pawnAttacks = pawnAttacks.removeLsb

            if position.enPassantSq.contains(to) then moves.add(Move(from, to, MoveType.EnPassant))
            else if to.lastRank(position.activeSide) then
              // these are only capture promotions
              addPromotions(from, to, isCapture = true, moves)
            else moves.add(Move(from, to, MoveType.Capture))
        else
          while attackMask.nonEmpty do
            val to = Square(attackMask.trailingZeros)
            attackMask = attackMask.removeLsb
            moves.add(Move(from, to, MoveType.Capture))
    }

    moves.toList

  def pseudoLegalMoves(state: GameState): List[Move] =
    val moves = MoveList(256)

    val occupied     = state.board.occupied
    val activePieces = state.board.byColor(state.activeSide)
    val targets      = state.board.byColor(state.activeSide.enemy)
    // this is the bulk of the moves. This should cover all of the attacks for every piece,
    // and then quiet moves for all pieces except for pawns
    Piece.byColor(state.activeSide) foreach { piece =>
      // handle pawns separately
      var pieces = state.board.pieces(piece)

      while pieces.nonEmpty do
        val from = Square(pieces.trailingZeros)
        pieces = pieces.removeLsb

        val rawAttacks = Attacks.attacks(piece, from, occupied)
        var attackMask = rawAttacks & targets
        var quietMask  = rawAttacks & ~occupied

        if piece == Piece.WhitePawn || piece == Piece.BlackPawn then
          var pawnAttacks = rawAttacks & (attackMask | (state.enPassant match
            case Some(sq) => 1L << sq.value
            case None     => 0L
          ))
          // first generate pawn attacks
          while pawnAttacks.nonEmpty do
            val to = Square(pawnAttacks.trailingZeros)
            pawnAttacks = pawnAttacks.removeLsb

            if state.enPassant.isDefined && to == state.enPassant.get then
              state.enPassant.foreach(sq => moves.add(Move(from, to, MoveType.EnPassant)))
            else if to.lastRank(state.activeSide) then addPromotions(from, to, isCapture = true, moves)
            else moves.add(Move(from, to, MoveType.Capture))

          // TODO: can i make this faster by breaking this out and doing all the pawns in one shot?
          // now handle pawn pushes
          var singlePush =
            ~occupied & (state.activeSide match
              case Color.White => (1L << from.value) << 8
              case Color.Black => (1L << from.value) >>> 8
            )
          var doublePush =
            ~occupied & (state.activeSide match
              case Color.White => singlePush << 8
              case Color.Black => singlePush >>> 8
            ) // (singlePush << direction)

          while singlePush.nonEmpty do
            val to = Square(singlePush.trailingZeros)
            singlePush = singlePush.removeLsb

            if to.lastRank(state.activeSide) then addPromotions(from, to, isCapture = false, moves)
            else moves.add(Move(from, to, MoveType.Quiet))

          if from.startingPawnRank(state.activeSide) then
            while doublePush.nonEmpty do
              val to = Square(doublePush.trailingZeros)
              doublePush = doublePush.removeLsb

              moves.add(Move(from, to, MoveType.DoublePush))
        else
          while attackMask.nonEmpty do
            val to = Square(attackMask.trailingZeros)
            attackMask = attackMask.removeLsb

            moves.add(Move(from, to, MoveType.Capture))

          while quietMask.nonEmpty do
            val to = Square(quietMask.trailingZeros)
            quietMask = quietMask.removeLsb

            moves.add(Move(from, to, MoveType.Quiet))
    }

    // castles
    if state.hasCastleRights then
      castleMoves(
        occupied,
        state.activeSide,
        state.castleRights,
        state.board,
        moves
      )

    moves.toList

  private def castleMovesMutable(
    occupied: Bitboard,
    activeSide: Color,
    castleRights: CastleRights,
    position: MutablePosition,
    moves: MoveList
  ): Unit =
    if castleRights.has(activeSide, CastleSide.Kingside)
    then kingsideCastlesMutable(occupied, activeSide, position, moves)
    if castleRights.has(activeSide, CastleSide.Queenside)
    then queensideCastlesMutable(occupied, activeSide, position, moves)

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

  private def kingsideCastlesMutable(
    occupied: Bitboard,
    activeSide: Color,
    position: MutablePosition,
    moves: MoveList
  ): Unit =
    val (squares, mask) =
      if activeSide == Color.White then (List(Square.F1, Square.G1), Bitboard.f1g1Mask)
      else (List(Square.F8, Square.G8), Bitboard.f8g8mask)

    val (from, to) =
      if activeSide == Color.White then (Square.E1, Square.G1)
      else (Square.E8, Square.G8)

    if squares.forall(!position.isSquareAttacked(_, activeSide.enemy))
      && (occupied & mask).isEmpty && !position.isSideInCheck(activeSide)
    then moves.add(Move(from, to, MoveType.CastleKingside))

  private def kingsideCastles(
    occupied: Bitboard,
    activeSide: Color,
    board: Board,
    moves: MoveList
  ): Unit =
    val (squares, mask) =
      if activeSide == Color.White then (List(Square.F1, Square.G1), Bitboard.f1g1Mask)
      else (List(Square.F8, Square.G8), Bitboard.f8g8mask)

    val (from, to) =
      if activeSide == Color.White then (Square.E1, Square.G1)
      else (Square.E8, Square.G8)

    if squares.forall(
        !board.isAttacked(_, activeSide)
      ) && (occupied & mask).isEmpty && !board.isKingAttacked(activeSide)
    then moves.add(Move(from, to, MoveType.CastleKingside))

  private def queensideCastlesMutable(
    occupied: Bitboard,
    activeSide: Color,
    position: MutablePosition,
    moves: MoveList
  ): Unit =
    val (squares, mask) =
      if activeSide == Color.White then (List(Square.C1, Square.D1), Bitboard.b1c1d1Mask)
      else (List(Square.C8, Square.D8), Bitboard.b8c8d8Mask)

    val (from, to) =
      if activeSide == Color.White then (Square.E1, Square.C1)
      else (Square.E8, Square.C8)

    if squares.forall(!position.isSquareAttacked(_, activeSide.enemy)) && (occupied & mask).isEmpty && !position.isSideInCheck(
        activeSide
      )
    then moves.add(Move(from, to, MoveType.CastleQueenside))

  private def queensideCastles(
    occupied: Bitboard,
    activeSide: Color,
    board: Board,
    moves: MoveList
  ): Unit =
    val (squares, mask) =
      if activeSide == Color.White then (List(Square.C1, Square.D1), Bitboard.b1c1d1Mask)
      else (List(Square.C8, Square.D8), Bitboard.b8c8d8Mask)

    val (from, to) =
      if activeSide == Color.White then (Square.E1, Square.C1)
      else (Square.E8, Square.C8)

    if squares.forall(
        !board.isAttacked(_, activeSide)
      ) && (occupied & mask).isEmpty && !board.isKingAttacked(activeSide)
    then moves.add(Move(from, to, MoveType.CastleQueenside))

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

    promotions foreach { pt => moves.add(Move(from, to, pt, moveType)) }
