package com.ffb.zugzwang.chess

import com.ffb.zugzwang.board.Bitboard
import com.ffb.zugzwang.chess.zobrist.{Zobrist, ZobristHash, ZobristKeys}
import com.ffb.zugzwang.move.{HQSlidingAttacks, KingAttacks, KnightAttacks, Move, MoveType, PawnAttacks}

final class MutablePosition(
  var pieces: Array[Bitboard],
  var squares: Array[Piece],
  var activeSide: Color,
  var enPassantSq: Option[Square],
  var castleRights: CastleRights,
  var halfMoveClock: Int,
  var fullMoveClock: Int,
  var zobristHash: ZobristHash = ZobristHash.empty
):
  // internal history stack
  private val maxDepth = 256
  private val history  = Array.fill(maxDepth)(new PositionUndoState)
  private var ply      = 0

  // cached info so we don't need to recompute all the time
  var occupied: Bitboard       = Bitboard.empty
  var byColor: Array[Bitboard] = Array(Bitboard.empty, Bitboard.empty)
  var kingSq: Array[Square]    = Array(Square.E1, Square.E8)

  rebuildCaches()
  zobristHash = Zobrist.compute(this)

  private def rebuildCaches(): Unit =
    occupied = Bitboard.empty
    byColor(0) = Bitboard.empty
    byColor(0) = Bitboard.empty

    var i = 0
    while i < 12 do
      occupied |= pieces(i)
      i += 1

    var sq = 0
    while sq < 64 do
      val p = squares(sq)
      if !p.isNoPiece then
        val square = Square(sq)
        val bb     = Bitboard.from(square)
        byColor(p.color.ordinal) |= bb

        if p.isKing then kingSq(p.color.ordinal) = square

      sq += 1

  def pieceAt(sq: Square): Piece = squares(sq.value)

  private def putPieceAt(piece: Piece, sq: Square): Unit =
    val sqIndex = sq.value
    squares(sqIndex) = piece

    val bb = Bitboard.from(sq)

    // cached info updates
    pieces(piece) = pieces(piece).setBitAt(sq)
    occupied |= bb
    byColor(piece.color.ordinal) |= bb

    if piece.isKing then kingSq(piece.color.ordinal) = sq

  private def removePieceAt(sq: Square): Piece =
    val sqIndex = sq.value
    val piece   = squares(sqIndex)

    if piece.isNoPiece then Piece.NoPiece
    else
      squares(sqIndex) = Piece.NoPiece

      val bb = Bitboard.from(sq)

      // cached info updates
      pieces(piece) = pieces(piece).clearBitAt(sq)
      occupied &= ~bb
      byColor(piece.color.ordinal) &= ~bb

      piece

  private def movePiece(from: Square, to: Square): Piece =
    val fromIndex = from.value
    val toIndex   = to.value
    val moving    = squares(fromIndex)

    squares(fromIndex) = Piece.NoPiece
    squares(toIndex) = moving

    val fromBB = Bitboard.from(from)
    val toBB   = Bitboard.from(to)

    // update the bitboard in the pieces array with the new location of the moving piece
    pieces(moving) = (pieces(moving) & ~fromBB) | toBB

    // cached info updates
    occupied = (occupied & ~fromBB) | toBB
    val c = moving.color.ordinal
    byColor(c) = (byColor(c) & ~fromBB) | toBB

    if moving.isKing then kingSq(c) = to

    moving

  private inline def rookHome(color: Color, side: CastleSide): Square =
    (color, side) match
      case (Color.White, CastleSide.Kingside)  => Square.H1
      case (Color.White, CastleSide.Queenside) => Square.A1
      case (Color.Black, CastleSide.Kingside)  => Square.H8
      case (Color.Black, CastleSide.Queenside) => Square.A8

  private def updateCastleRightsOnMove(moved: Piece, from: Square, captured: Piece, to: Square): Unit =
    if moved.isKing then castleRights = castleRights.removeAll(moved.color)
    else if moved.isRook then
      val color = moved.color
      if from == rookHome(color, CastleSide.Kingside) then castleRights = castleRights.remove(color, CastleSide.Kingside)
      else if from == rookHome(color, CastleSide.Queenside) then castleRights = castleRights.remove(color, CastleSide.Queenside)

    if captured.isRook then
      val color = captured.color
      if to == rookHome(color, CastleSide.Kingside) then castleRights = castleRights.remove(color, CastleSide.Kingside)
      else if to == rookHome(color, CastleSide.Queenside) then castleRights = castleRights.remove(color, CastleSide.Queenside)

  def applyMove(move: Move): Unit =
    val state = history(ply)

    state.prevZobristHash = zobristHash
    state.prevCastleRights = castleRights
    state.prevEnPassant = enPassantSq
    state.prevHalfMove = halfMoveClock
    state.prevFullMove = fullMoveClock

    val oldCastleMask = castleRights.maskValue

    if enPassantSq.nonEmpty then
      val sq = enPassantSq.get
      // TODO: look at the performance of Square.file
      zobristHash ^= ZobristKeys.epFile(Square.file(sq).value)

    zobristHash ^= ZobristKeys.sideToMove

    // clear ep by default (may be set again below)
    enPassantSq = None

    val from  = move.from
    val to    = move.to
    val moved = squares(from.value)
    state.movedPiece = moved

    state.captured = Piece.NoPiece
    state.capturedSquare = to

    var didCapture = false

    // zobrist: moving piece leaves from square
    // (always happens for any legal move)
    zobristHash ^= ZobristKeys.pieceSquare(moved)(from.value)

    move.moveType match
      case MoveType.CastleKingside =>
        removePieceAt(to)
        movePiece(from, to)

        // zobrist: king appears on to square
        zobristHash ^= ZobristKeys.pieceSquare(moved)(to.value)

        val (rookFrom, rookTo) =
          if moved.color == Color.White then (Square.H1, Square.F1)
          else (Square.H8, Square.F8)

        val rook = squares(rookFrom.value) // should be the rook
        // rook moves rookFrom -> rookTo
        zobristHash ^= ZobristKeys.pieceSquare(rook)(rookFrom.value)
        zobristHash ^= ZobristKeys.pieceSquare(rook)(rookTo.value)

        movePiece(rookFrom, rookTo)

        // castling rights are removed when king castles
        castleRights = castleRights.removeAll(activeSide)

      case MoveType.CastleQueenside =>
        removePieceAt(to)
        movePiece(from, to)

        // zobrist: king appears on to square
        zobristHash ^= ZobristKeys.pieceSquare(moved)(to.value)

        val (rookFrom, rookTo) =
          if activeSide == Color.White then (Square.A1, Square.D1)
          else (Square.A8, Square.D8)

        val rook = squares(rookFrom.value)
        zobristHash ^= ZobristKeys.pieceSquare(rook)(rookFrom.value)
        zobristHash ^= ZobristKeys.pieceSquare(rook)(rookTo.value)

        movePiece(rookFrom, rookTo)

        castleRights = castleRights.removeAll(activeSide)

      case MoveType.EnPassant =>
        // ep capture: captured pawn is *behind* the to-square
        val capturedSquare =
          if activeSide == Color.White then Square(to.value - 8)
          else Square(to.value + 8)

        state.capturedSquare = capturedSquare

        val capturedPawn = squares(capturedSquare.value)
        // zobrist: remove captured pawn from capturedSquare
        if !capturedPawn.isNoPiece then zobristHash ^= ZobristKeys.pieceSquare(capturedPawn)(capturedSquare.value)

        val captured = removePieceAt(capturedSquare)
        state.captured = captured
        didCapture = true

        removePieceAt(to)
        movePiece(from, to)

        // zobrist: moving pawn appears on to square
        zobristHash ^= ZobristKeys.pieceSquare(moved)(to.value)

      case _ =>
        val captured = removePieceAt(to)
        if !captured.isNoPiece then
          // zobrist: remove captured piece from to square
          zobristHash ^= ZobristKeys.pieceSquare(captured)(to.value)
          state.captured = captured
          didCapture = true

        movePiece(from, to)

        if move.isPromotion then
          val promoPiece = Piece.from(activeSide, move.promotion)

          zobristHash ^= ZobristKeys.pieceSquare(promoPiece)(to.value)

          removePieceAt(to)
          putPieceAt(promoPiece, to)
        else
          // non-promotion: moving piece appears on to square
          zobristHash ^= ZobristKeys.pieceSquare(moved)(to.value)

        // double push sets EP square
        if move.moveType == MoveType.DoublePush then
          val epSquare =
            if activeSide == Color.White then Square(to.value - 8)
            else Square(to.value + 8)
          enPassantSq = Some(epSquare)

        updateCastleRightsOnMove(moved, from, state.captured, to)

    // clocks
    if moved.isPawn || didCapture then halfMoveClock = 0
    else halfMoveClock += 1

    if activeSide == Color.White then fullMoveClock else fullMoveClock += 1

    // zobrist: add new state-dependent runes (castling + ep)
    val newCastleMask = castleRights.maskValue
    if newCastleMask != oldCastleMask then
      zobristHash ^= ZobristKeys.castling(oldCastleMask)
      zobristHash ^= ZobristKeys.castling(newCastleMask)

    if enPassantSq.nonEmpty then
      val sq = enPassantSq.get
      zobristHash ^= ZobristKeys.epFile(Square.file(sq).value)

    // flip side to move in the position
    activeSide = activeSide.enemy
    ply += 1

  // DEBUG: uncomment the line below to test
  // assert(zobristHash == Zobrist.compute(this))

  def unapplyMove(move: Move): Unit =
    ply -= 1
    val state = history(ply)

    zobristHash = state.prevZobristHash

    activeSide = activeSide.enemy

    castleRights = state.prevCastleRights
    enPassantSq = state.prevEnPassant
    halfMoveClock = state.prevHalfMove
    fullMoveClock = state.prevFullMove

    val from = move.from
    val to   = move.to

    move.moveType match
      case MoveType.CastleKingside =>
        val (rookFrom, rookTo) =
          if activeSide == Color.White then (Square.F1, Square.H1)
          else (Square.F8, Square.H8)
        movePiece(rookFrom, rookTo)

        movePiece(to, from)

      case MoveType.CastleQueenside =>
        val (rookFrom, rookTo) =
          if activeSide == Color.White then (Square.D1, Square.A1)
          else (Square.D8, Square.A8)
        movePiece(rookFrom, rookTo)

        movePiece(to, from)

      case MoveType.EnPassant =>
        movePiece(to, from)

        if !state.captured.isNoPiece then putPieceAt(state.captured, state.capturedSquare)

      case _ =>
        if move.isPromotion then
          removePieceAt(to)
          putPieceAt(state.movedPiece, from)
        else movePiece(to, from)

        if !state.captured.isNoPiece then putPieceAt(state.captured, state.capturedSquare)

  private val sliders = HQSlidingAttacks

  def isSideInCheck(side: Color): Boolean =
    val sq = kingSq(side.ordinal)
    isSquareAttacked(sq, side.enemy)

  def isSquareAttacked(sq: Square, byColor: Color): Boolean =

    val occNow = occupied

    if byColor == Color.White then
      if (PawnAttacks.black(sq.value) & pieces(Piece.WhitePawn)).nonEmpty then return true
      if (KnightAttacks.table(sq.value) & pieces(Piece.WhiteKnight)).nonEmpty then return true

      val diag = sliders.bishopAttacks(sq, occNow)
      if (diag & (pieces(Piece.WhiteBishop) | pieces(Piece.WhiteQueen))).nonEmpty then return true

      val ortho = sliders.rookAttacks(sq, occNow)
      if (ortho & (pieces(Piece.WhiteRook) | pieces(Piece.WhiteQueen))).nonEmpty then return true

      if (KingAttacks.table(sq.value) & pieces(Piece.WhiteKing)).nonEmpty then return true

      false
    else
      if (PawnAttacks.white(sq.value) & pieces(Piece.BlackPawn)).nonEmpty then return true
      if (KnightAttacks.table(sq.value) & pieces(Piece.BlackKnight)).nonEmpty then return true

      val diag = sliders.bishopAttacks(sq, occNow)
      if (diag & (pieces(Piece.BlackBishop) | pieces(Piece.BlackQueen))).nonEmpty then return true

      val ortho = sliders.rookAttacks(sq, occNow)
      if (ortho & (pieces(Piece.BlackRook) | pieces(Piece.BlackQueen))).nonEmpty then return true

      if (KingAttacks.table(sq.value) & pieces(Piece.BlackKing)).nonEmpty then return true

      false

object MutablePosition:
  def from(state: GameState): MutablePosition =
    new MutablePosition(
      Array.from(state.board.pieces),
      Array.from(state.board.squares),
      state.activeSide,
      state.enPassant,
      state.castleRights,
      state.halfMoveClock,
      state.fullMoveClock
    )
