package com.ffb.zugzwang.evaluation

import com.ffb.zugzwang.chess.{Color, PieceType, Square}

object PieceSquareTables:
  private val PawnTable = Array(
    0, 0, 0, 0, 0, 0, 0, 0,         // Rank 1
    5, 10, 10, -20, -20, 10, 10, 5, // Rank 2
    5, -5, -10, 0, 0, -10, -5, 5,   // Rank 3
    0, 0, 0, 20, 20, 0, 0, 0,       // Rank 4 (Center Control!)
    5, 5, 10, 25, 25, 10, 5, 5,     // Rank 5
    10, 10, 20, 30, 30, 20, 10, 10, // Rank 6
    50, 50, 50, 50, 50, 50, 50, 50, // Rank 7 (About to promote)
    0, 0, 0, 0, 0, 0, 0, 0          // Rank 8 (Promoted, shouldn't exist)
  )

  private val KnightTable = Array(
    -50, -40, -30, -30, -30, -30, -40, -50, // Rank 1
    -40, -20, 0, 5, 5, 0, -20, -40,         // Rank 2
    -30, 5, 10, 15, 15, 10, 5, -30,         // Rank 3
    -30, 0, 15, 20, 20, 15, 0, -30,         // Rank 4
    -30, 5, 15, 20, 20, 15, 5, -30,         // Center is good (rank 5)
    -30, 0, 10, 15, 15, 10, 0, -30,         // Rank 6
    -40, -20, 0, 0, 0, 0, -20, -40,         // Rank 7
    -50, -40, -30, -30, -30, -30, -40, -50
  )

  private val BishopTable = Array(
    -20, -10, -10, -10, -10, -10, -10, -20, // Rank 1
    -10, 5, 0, 0, 0, 0, 5, -10,             // Rank 2
    -10, 10, 10, 10, 10, 10, 10, -10,       // Rank 3
    -10, 0, 10, 10, 10, 10, 0, -10,         // Rank 4
    -10, 5, 5, 10, 10, 5, 5, -10,           // Rank 5
    -10, 0, 5, 10, 10, 5, 0, -10,           // Rank 6
    -10, 0, 0, 0, 0, 0, 0, -10,             // Rank 7
    -20, -10, -10, -10, -10, -10, -10, -20  // Rank 8
  )

  private val RookTable = Array(
    0, 0, 0, 5, 5, 0, 0, 0,           // Rank 1
    -5, 0, 0, 0, 0, 0, 0, -5,         // Rank 2
    -5, 0, 0, 0, 0, 0, 0, -5,         // Rank 3
    -5, 0, 0, 0, 0, 0, 0, -5,         // Rank 4
    -5, 0, 0, 0, 0, 0, 0, -5,         // Rank 5
    -5, 0, 0, 0, 0, 0, 0, -5,         // Rank 6
    5, 10, 10, 10, 10, 10, 10, 10, 5, // Rank 7
    0, 0, 0, 0, 0, 0, 0, 0            // Rank 8
  )

  private val QueenTable = Array(
    -20, -10, -10, -5, -5, -10, -10, -20, // Rank 1
    -10, 0, 0, 0, 0, 5, 0, -10,           // Rank 2
    -10, 0, 5, 5, 5, 5, 5, -10,           // Rank 3
    -5, 0, 5, 5, 5, 5, 0, 0,              // Rank 4
    -5, 0, 5, 5, 5, 5, 0, -5,             // Rank 5
    -10, 0, 5, 5, 5, 5, 0, -10,           // Rank 6
    -10, 0, 0, 0, 0, 0, 0, -10,           // Rank 7
    -20, -10, -10, -5, -5, -10, -10, -20  // Rank 8
  )

  private val KingTable = Array(
    20, 30, 10, 0, 0, 10, 30, 20,           // Rank 1
    20, 20, 0, 0, 0, 0, 20, 20,             // Rank 2
    -10, -20, -20, -20, -20, -20, -20, -10, // Rank 3
    -20, -30, -30, -40, -40, -30, -30, -20, // Rank 4
    -30, -40, -40, -50, -50, -40, -40, -30, // Rank 5
    -30, -40, -40, -50, -50, -40, -40, -30, // Rank 6
    -30, -40, -40, -50, -50, -40, -40, -30, // Rank 7
    -30, -40, -40, -50, -50, -40, -40, -30  // Rank 8
  )

  def value(pt: PieceType, sq: Square, color: Color): Int =
    val table = pt match
      case PieceType.Pawn   => PawnTable
      case PieceType.Knight => KnightTable
      case PieceType.Bishop => BishopTable
      case PieceType.Rook   => RookTable
      case PieceType.Queen  => QueenTable
      case PieceType.King   => KingTable
      case _                => return 0

    // Rank mirroring: square ^ 56  (XOR 56 flips the rank index 0->7, 1->6)
    val index = if color == Color.White then sq.value else (sq.value ^ 56)

    if index >= 0 && index < 64 then table(index) else 0
