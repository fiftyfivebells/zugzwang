package com.ffb.zugzwang.move

import com.ffb.zugzwang.chess.{PieceType, Square}

enum MoveType:
  case Quiet,
    DoublePush,
    Capture,
    CastleKingside,
    CastleQueenside,
    EnPassant,
    Promotion,
    CapturePromotion

  override def toString: String = productPrefix.toLowerCase

opaque type Move = Int
object Move:
  private val squareMask = 63
  private val toShift    = 6

  private val pieceTypeMask       = 7
  private val promotionPieceShift = 12

  private val moveTypeMask  = 7
  private val moveTypeShift = 15

  def apply(
    from: Square,
    to: Square,
    moveType: MoveType
  ): Move =
    apply(from, to, PieceType.NoType, moveType)

  def apply(
    from: Square,
    to: Square,
    promotion: PieceType,
    moveType: MoveType
  ): Move =
    from.value | (to.value << toShift) | (promotion << promotionPieceShift) | (moveType.ordinal << moveTypeShift)

  def unapply(move: Move): Option[(Square, Square, PieceType, MoveType)] =
    Some((move.from, move.to, move.promotion, move.moveType))

  extension (move: Move)
    inline def from: Square = Square(move & squareMask)

    inline def to: Square = Square((move >>> toShift) & squareMask)

    inline def promotion: PieceType =
      val piece = (move >>> promotionPieceShift) & pieceTypeMask
      if piece == 0 then PieceType.NoType else PieceType(piece)

    inline def moveType: MoveType =
      MoveType.fromOrdinal((move >>> moveTypeShift) & moveTypeMask)

    inline def isCapture: Boolean =
      val mt = moveType
      mt == MoveType.Capture || mt == MoveType.EnPassant || mt == MoveType.CapturePromotion

    inline def isPromotion: Boolean =
      val mt = moveType
      mt == MoveType.Promotion || mt == MoveType.CapturePromotion

    def toUci: String =
      val (f, t) = (Square.toAlgebraic(from), Square.toAlgebraic(to))
      s"$f$t${move.promotion.name}"
