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
    ml.clear()

    val occupied   = position.occupied
    val targets    = position.byColor(position.activeSide.enemy.ordinal)
    val epBitboard = if position.enPassantSq.isDefined then 1L << position.enPassantSq.toInt else 0L

    val piecesArr = Piece.byColor(position.activeSide)
    var pieceIdx  = 0
    while pieceIdx < piecesArr.length do
      val piece  = piecesArr(pieceIdx)
      var pieces = position.pieces(piece)

      while pieces.nonEmpty do
        val from = Square(pieces.trailingZeros)
        pieces = pieces.removeLsb

        val rawAttacks = Attacks.attacks(piece, from, occupied)
        var attackMask = rawAttacks & targets
        var quietMask  = rawAttacks & ~occupied

        if piece == Piece.WhitePawn || piece == Piece.BlackPawn then
          var pawnAttacks = rawAttacks & (attackMask | epBitboard)
          // first generate pawn attacks
          while pawnAttacks.nonEmpty do
            val to = Square(pawnAttacks.trailingZeros)
            pawnAttacks = pawnAttacks.removeLsb

            if (epBitboard & (1L << to.toInt)) != 0L then ml.add(Move(from, to, MoveType.EnPassant))
            else if to.lastRank(position.activeSide) then addPromotions(from, to, isCapture = true, ml)
            else ml.add(Move(from, to, MoveType.Capture))

          // TODO: can i make this faster by breaking this out and doing all the pawns in one shot?
          // now handle pawn pushes
          var singlePush =
            ~occupied & (position.activeSide match
              case Color.White => (1L << from.toInt) << 8
              case Color.Black => (1L << from.toInt) >>> 8
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
      pieceIdx += 1

    // castles
    if !position.castleRights.isEmpty then castleMoves(occupied, position.activeSide, position.castleRights, position, ml)

  /** Pseudolegal captures only. Used by quiescence search. */
  def fillCaptures(position: MutablePosition, ml: MoveList): Unit =
    ml.clear()

    val occupied   = position.occupied
    val targets    = position.byColor(position.activeSide.enemy.ordinal)
    val epBitboard = if position.enPassantSq.isDefined then 1L << position.enPassantSq.toInt else 0L

    val piecesArr = Piece.byColor(position.activeSide)
    var pieceIdx  = 0
    while pieceIdx < piecesArr.length do
      val piece  = piecesArr(pieceIdx)
      var pieces = position.pieces(piece)

      while pieces.nonEmpty do
        val from = Square(pieces.trailingZeros)
        pieces = pieces.removeLsb

        val rawAttacks = Attacks.attacks(piece, from, occupied)
        var attackMask = rawAttacks & targets

        if piece.isPawn then
          var pawnAttacks = attackMask | (rawAttacks & epBitboard)

          while pawnAttacks.nonEmpty do
            val to = Square(pawnAttacks.trailingZeros)
            pawnAttacks = pawnAttacks.removeLsb

            if (epBitboard & (1L << to.toInt)) != 0L then ml.add(Move(from, to, MoveType.EnPassant))
            else if to.lastRank(position.activeSide) then
              // these are only capture promotions
              addPromotions(from, to, isCapture = true, ml)
            else ml.add(Move(from, to, MoveType.Capture))
        else
          while attackMask.nonEmpty do
            val to = Square(attackMask.trailingZeros)
            attackMask = attackMask.removeLsb
            ml.add(Move(from, to, MoveType.Capture))
      pieceIdx += 1

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
    val enemy = activeSide.enemy
    if activeSide == Color.White then
      if !position.pieceAt(Square.H1).isNoPiece
        && !position.isSquareAttacked(Square.F1, enemy)
        && !position.isSquareAttacked(Square.G1, enemy)
        && (occupied & Bitboard.f1g1Mask).isEmpty
        && !position.isSideInCheck(activeSide)
      then ml.add(Move(Square.E1, Square.G1, MoveType.CastleKingside))
    else if !position.pieceAt(Square.H8).isNoPiece
      && !position.isSquareAttacked(Square.F8, enemy)
      && !position.isSquareAttacked(Square.G8, enemy)
      && (occupied & Bitboard.f8g8mask).isEmpty
      && !position.isSideInCheck(activeSide)
    then ml.add(Move(Square.E8, Square.G8, MoveType.CastleKingside))

  private def queensideCastle(
    occupied: Bitboard,
    activeSide: Color,
    position: MutablePosition,
    ml: MoveList
  ): Unit =
    val enemy = activeSide.enemy
    if activeSide == Color.White then
      if !position.pieceAt(Square.A1).isNoPiece
        && !position.isSquareAttacked(Square.C1, enemy)
        && !position.isSquareAttacked(Square.D1, enemy)
        && (occupied & Bitboard.b1c1d1Mask).isEmpty
        && !position.isSideInCheck(activeSide)
      then ml.add(Move(Square.E1, Square.C1, MoveType.CastleQueenside))
    else if !position.pieceAt(Square.A8).isNoPiece
      && !position.isSquareAttacked(Square.C8, enemy)
      && !position.isSquareAttacked(Square.D8, enemy)
      && (occupied & Bitboard.b8c8d8Mask).isEmpty
      && !position.isSideInCheck(activeSide)
    then ml.add(Move(Square.E8, Square.C8, MoveType.CastleQueenside))

  private def addPromotions(
    from: Square,
    to: Square,
    isCapture: Boolean,
    ml: MoveList
  ): Unit =
    val moveType =
      if isCapture then MoveType.CapturePromotion else MoveType.Promotion

    ml.add(Move(from, to, PieceType.Queen, moveType))
    ml.add(Move(from, to, PieceType.Rook, moveType))
    ml.add(Move(from, to, PieceType.Bishop, moveType))
    ml.add(Move(from, to, PieceType.Knight, moveType))
