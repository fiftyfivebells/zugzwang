package com.ffb.zugzwang.notation

import com.ffb.zugzwang.board.Board
import com.ffb.zugzwang.chess.{CastleRights, Color, GameState, Square}

import scala.util.Try

object FENParser:

  def from(fen: String): Either[FENParserError, GameState] =
    fen.split(" ") match
      case Array(
            boardStr,
            sideStr,
            castleStr,
            epStr,
            halfMoveStr,
            fullMoveStr
          ) =>
        for
          board        <- parseBoard(boardStr)
          side         <- parseSideStr(sideStr)
          castleRights <- parseCastleStr(castleStr)
          enPassant    <- parseEnPassantStr(epStr)
          halfMove     <- parseMoveCountStr(halfMoveStr, "half move")
          fullMove     <- parseMoveCountStr(fullMoveStr, "full move")
        yield GameState(
          board = board,
          activeSide = side,
          castleRights = castleRights,
          enPassant = enPassant,
          halfMoveClock = halfMove,
          fullMoveClock = fullMove,
          history = Nil
        )
      case _ => Left(FENParserError.MalformedInput(fen))

  private def parseBoard(boardStr: String): Either[FENParserError, Board] =
    val ranks = boardStr.split("/")

    if ranks.length != 8 then Left(FENParserError.BoardMissingRanks(boardStr))
    else if !ranks.forall(r => sumRank(r) < 9) then Left(FENParserError.BoardRankMalformed(boardStr))
    else if !ranks.forall(validChars) then Left(FENParserError.BoardInvalidChars(boardStr))
    else if rankContainsPawn(ranks(0)) || rankContainsPawn(ranks(7)) then Left(FENParserError.BoardInvalidPawnRank(boardStr))
    else Right(Board.from(boardStr))

  private def sumRank(rank: String): Int =
    rank.foldLeft(0) { (acc, c) =>
      if c.isDigit then acc + c.asDigit
      else acc + 1
    }

  private def validChars(rank: String): Boolean =
    val validChars = "PNBRQKpnbrqk12345678"

    rank.forall(c => validChars.contains(c))

  private def rankContainsPawn(rank: String): Boolean =
    rank.contains("p") || rank.contains("P")

  private def parseSideStr(sideStr: String): Either[FENParserError, Color] =
    sideStr match
      case "w" => Right(Color.White)
      case "b" => Right(Color.Black)
      case _   => Left(FENParserError.InvalidActiveSide(sideStr))

  private def parseCastleStr(
    castleStr: String
  ): Either[FENParserError, CastleRights] =
    val allRights = "KQkq-"

    if castleStr.length > 4 then Left(FENParserError.CastleRightsTooLong(castleStr))
    else if !castleStr.forall(c => allRights.contains(c)) then Left(FENParserError.CastleRightsInvalidChar(castleStr))
    else Right(CastleRights.from(castleStr))

  private def parseEnPassantStr(
    epStr: String
  ): Either[FENParserError, Option[Square]] =
    if epStr == "-" then Right(None)
    else
      Square.fromAlgebraic(epStr) match
        case Left(_) => Left(FENParserError.EPSquareInvalid(epStr))
        case Right(sq) =>
          if sq.rank.value == 2 || sq.rank.value == 5 then Right(Some(sq))
          else Left(FENParserError.EPSquareBadRank(epStr))

  private def parseMoveCountStr(
    moveCountStr: String,
    countType: String
  ): Either[FENParserError, Int] =
    Try(moveCountStr.toInt).toEither.left.map(_ => FENParserError.BadMoveCount(countType, moveCountStr))

end FENParser

enum FENParserError(val errorMessage: String) extends Throwable:
  case MalformedInput(value: String)       extends FENParserError(s"Received malformed FEN string: $value")
  case BoardMissingRanks(value: String)    extends FENParserError(s"Board string must have 8 ranks: $value")
  case BoardRankMalformed(value: String)   extends FENParserError(s"Board contains invalid ranks: $value")
  case BoardInvalidChars(value: String)    extends FENParserError(s"Board contains invalid chars: $value")
  case BoardInvalidPawnRank(value: String) extends FENParserError(s"Board contains pawn on last rank: $value")
  case InvalidActiveSide(value: String)    extends FENParserError(s"Invalid active side: $value")
  case CastleRightsTooLong(value: String)
      extends FENParserError(
        s"Castle rights string has too many values: $value"
      )
  case CastleRightsInvalidChar(value: String)
      extends FENParserError(
        s"Invalid character in castle rights string: $value"
      )
  case EPSquareInvalid(value: String) extends FENParserError(s"En passant square was invalid: $value")
  case EPSquareBadRank(value: String)
      extends FENParserError(
        s"En passant square was on an invalid rank: $value"
      )
  case BadMoveCount(countType: String, value: String) extends FENParserError(s"Invalid $countType value: $value")

end FENParserError
