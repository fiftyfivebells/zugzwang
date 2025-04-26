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

final case class Move(
    from: Square,
    to: Square,
    pieceType: PieceType,
    promotionPieceType: Option[PieceType],
    moveType: MoveType
):
  // TODO: i may want a different string implementation, but this is ok for now
  override def toString: String = Move.toUci(this)
end Move

object Move:
  def isAttack(move: Move): Boolean =
    val attackMoves =
      Set(MoveType.Capture, MoveType.EnPassant, MoveType.CapturePromotion)

    attackMoves.contains(move.moveType)

  def isPromotion(move: Move): Boolean =
    val promotionMoves = Set(MoveType.Promotion, MoveType.CapturePromotion)

    promotionMoves.contains(move.moveType)

  def toUci(move: Move): String =
    val promotion = move.promotionPieceType match {
      case Some(p) => p.toString
      case None    => ""
    }

    s"${move.from.toString}${move.to.toString}$promotion"

end Move
