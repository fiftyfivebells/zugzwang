package com.ffb.zugzwang.move

import com.ffb.zugzwang.chess.{PieceType, Square}

enum MoveType:
  case Quiet,
    Capture,
    CastleKingside,
    CastleQueenside,
    EnPassant,
    Promotion,
    CapturePromotion

opaque type Move = Int

object Move:
  private val squareMask = 63
  private val toShift = 6

  private val pieceTypeMask = 7
  private val promotionPieceShift = 12

  private val moveTypeMask = 6
  private val moveTypeShift = 15

  def apply(
      from: Square,
      to: Square,
      promotion: Option[PieceType],
      moveType: MoveType
  ): Move =
    val promotionValue = promotion.map(_.ordinal).getOrElse(0)
    from.value | (to.value << toShift) | (promotionValue << promotionPieceShift) | (moveType.ordinal << moveTypeShift)

  extension (move: Move)
    inline def from: Square = Square(move & squareMask)

    inline def to: Square = Square((move >>> toShift) & squareMask)

    def promotion: Option[PieceType] =
      val piece = (move >>> promotionPieceShift) & pieceTypeMask
      if piece == 0 then None else Some(PieceType.fromOrdinal(piece))

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
      promotion match {
        case None    => s"$f$t"
        case Some(p) => s"$f$t${p.name}"
      }

end Move
