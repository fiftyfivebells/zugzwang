package com.ffb.zugzwang.search

import com.ffb.zugzwang.board.Bitboard
import com.ffb.zugzwang.chess.{CastleRights, CastleSide, Color, MutablePosition, Piece, PieceType, Square}
import com.ffb.zugzwang.move.{Attacks, Move, MoveList, MoveType}

object SearchMoveGen:

  /**
   * All pseudolegal moves. Caller filters for legality (king not left in
   * check).
   */
  def fillMoveList(position: MutablePosition, ml: MoveList): Unit =
    ml.clear

    val occupied = position.occupied
    val targets  = position.byColor(position.activeSide.enemy.ordinal)
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
              position.enPassantSq.foreach(sq => ml.add(Move(from, to, MoveType.EnPassant)))
            else if to.lastRank(position.activeSide) then addPromotions(from, to, isCapture = true, ml)
            else ml.add(Move(from, to, MoveType.Capture))

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

            if to.lastRank(position.activeSide) then addPromotions(from, to, isCapture = false, ml)
            else ml.add(Move(from, to, MoveType.Quiet))

          if from.startingPawnRank(position.activeSide) then
            while doublePush.nonEmpty do
              val to = Square(doublePush.trailingZeros)
              doublePush = doublePush.removeLsb

              ml.add(Move(from, to, MoveType.DoublePush))
        else
          while attackMask.nonEmpty do
            val to = Square(attackMask.trailingZeros)
            attackMask = attackMask.removeLsb

            ml.add(Move(from, to, MoveType.Capture))

          while quietMask.nonEmpty do
            val to = Square(quietMask.trailingZeros)
            quietMask = quietMask.removeLsb

            ml.add(Move(from, to, MoveType.Quiet))
    }

    // castles
    if !position.castleRights.isEmpty then castleMoves(occupied, position.activeSide, position.castleRights, position, ml)

  /** Pseudolegal captures only. Used by quiescence search. */
  def fillCaptures(position: MutablePosition, ml: MoveList): Unit =
    ml.clear

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

            if position.enPassantSq.contains(to) then ml.add(Move(from, to, MoveType.EnPassant))
            else if to.lastRank(position.activeSide) then
              // these are only capture promotions
              addPromotions(from, to, isCapture = true, ml)
            else ml.add(Move(from, to, MoveType.Capture))
        else
          while attackMask.nonEmpty do
            val to = Square(attackMask.trailingZeros)
            attackMask = attackMask.removeLsb
            ml.add(Move(from, to, MoveType.Capture))
    }

  private def castleMoves(
    occupied: Bitboard,
    activeSide: Color,
    castleRights: CastleRights,
    position: MutablePosition,
    ml: MoveList
  ): Unit =
    if castleRights.has(activeSide, CastleSide.Kingside)
    then kingsideCastle(occupied, activeSide, position, ml)
    if castleRights.has(activeSide, CastleSide.Queenside)
    then queensideCastle(occupied, activeSide, position, ml)

  private def kingsideCastle(
    occupied: Bitboard,
    activeSide: Color,
    position: MutablePosition,
    ml: MoveList
  ): Unit =
    val (squares, mask) =
      if activeSide == Color.White then (List(Square.F1, Square.G1), Bitboard.f1g1Mask)
      else (List(Square.F8, Square.G8), Bitboard.f8g8mask)

    val (from, to) =
      if activeSide == Color.White then (Square.E1, Square.G1)
      else (Square.E8, Square.G8)

    if squares.forall(!position.isSquareAttacked(_, activeSide.enemy))
      && (occupied & mask).isEmpty && !position.isSideInCheck(activeSide)
    then ml.add(Move(from, to, MoveType.CastleKingside))

  private def queensideCastle(
    occupied: Bitboard,
    activeSide: Color,
    position: MutablePosition,
    ml: MoveList
  ): Unit =
    val (squares, mask) =
      if activeSide == Color.White then (List(Square.C1, Square.D1), Bitboard.b1c1d1Mask)
      else (List(Square.C8, Square.D8), Bitboard.b8c8d8Mask)

    val (from, to) =
      if activeSide == Color.White then (Square.E1, Square.C1)
      else (Square.E8, Square.C8)

    if squares.forall(!position.isSquareAttacked(_, activeSide.enemy))
      && (occupied & mask).isEmpty && !position.isSideInCheck(activeSide)
    then ml.add(Move(from, to, MoveType.CastleQueenside))

  private def addPromotions(
    from: Square,
    to: Square,
    isCapture: Boolean,
    ml: MoveList
  ): Unit =
    val moveType =
      if isCapture then MoveType.CapturePromotion else MoveType.Promotion

    val promotions =
      List(PieceType.Knight, PieceType.Bishop, PieceType.Rook, PieceType.Queen)

    promotions foreach { pt => ml.add(Move(from, to, pt, moveType)) }
