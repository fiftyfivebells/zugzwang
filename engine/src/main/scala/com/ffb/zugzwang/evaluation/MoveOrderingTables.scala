package com.ffb.zugzwang.evaluation

import com.ffb.zugzwang.chess.{Piece, Square}

// PST values used exclusively for move ordering (quiet move scoring in MoveSorter).
// Intentionally kept as a copy of the original PeSTO midgame tables so that
// evaluation PSTs can evolve independently without affecting move ordering.
object MoveOrderingTables:

  val tables = Array(
    // pawns
    Array(
      0, 0, 0, 0, 0, 0, 0, 0,              // Rank 8
      98, 134, 61, 95, 68, 126, 34, -11,   // Rank 7
      -6, 7, 26, 31, 65, 56, 25, -20,      // Rank 6
      -14, 13, 6, 21, 23, 12, 17, -23,     // Rank 5
      -27, -2, -5, 12, 17, 6, 10, -25,     // Rank 4
      -26, -4, -4, -10, 3, 3, 33, -12,     // Rank 3
      -35, -1, -20, -23, -15, 24, 38, -22, // Rank 2
      0, 0, 0, 0, 0, 0, 0, 0               // Rank 1
    ),
    // knights
    Array(
      -167, -89, -34, -49, 61, -97, -15, -107, // Rank 8
      -73, -41, 72, 36, 23, 62, 7, -17,        // Rank 7
      -47, 60, 37, 65, 84, 129, 73, 44,        // Rank 6
      -9, 17, 19, 53, 37, 69, 18, 22,          // Rank 5
      -13, 4, 16, 13, 28, 19, 21, -8,          // Rank 4
      -23, -9, 12, 10, 19, 17, 25, -16,        // Rank 3
      -29, -53, -12, -3, -1, 18, -14, -19,     // Rank 2
      -105, -21, -58, -33, -17, -28, -19, -23  // Rank 1
    ),
    // bishops
    Array(
      -29, 4, -82, -37, -25, -42, 7, -8,    // Rank 8
      -26, 16, -18, -13, 30, 59, 18, -47,   // Rank 7
      -16, 37, 43, 40, 35, 50, 37, -2,      // Rank 6
      -4, 5, 19, 50, 37, 37, 7, -2,         // Rank 5
      -6, 13, 13, 26, 34, 12, 10, 4,        // Rank 4
      0, 15, 15, 15, 14, 27, 18, 10,        // Rank 3
      4, 15, 16, 0, 7, 21, 33, 1,           // Rank 2
      -33, -3, -14, -21, -13, -12, -39, -21 // Rank 1
    ),
    // rooks
    Array(
      32, 42, 32, 51, 63, 9, 31, 43,      // Rank 8
      27, 32, 58, 62, 80, 67, 26, 44,     // Rank 7
      -5, 19, 26, 36, 17, 45, 61, 16,     // Rank 6
      -24, -11, 7, 26, 24, 35, -8, -20,   // Rank 5
      -36, -26, -12, -1, 9, -7, 6, -23,   // Rank 4
      -45, -25, -16, -17, 3, 0, -5, -33,  // Rank 3
      -44, -16, -20, -9, -1, 11, -6, -71, // Rank 2
      -19, -13, 1, 17, 16, 7, -37, -26    // Rank 1
    ),
    // queens
    Array(
      -28, 0, 29, 12, 59, 44, 43, 45,      // Rank 8
      -24, -39, -5, 1, -16, 57, 28, 54,    // Rank 7
      -13, -17, 7, 8, 29, 56, 47, 57,      // Rank 6
      -27, -27, -16, -16, -1, 17, -2, 1,   // Rank 5
      -9, -26, -9, -10, -2, -4, 3, -3,     // Rank 4
      -14, 2, -11, -2, -5, 2, 14, 5,       // Rank 3
      -35, -8, 11, 2, 8, 15, -3, 1,        // Rank 2
      -1, -18, -9, -10, -30, -15, -16, -28 // Rank 1
    ),
    // kings
    Array(
      -65, 23, 16, -15, -56, -34, 2, 13,      // Rank 8
      29, -1, -20, -7, -8, -4, -38, -29,      // Rank 7
      -9, 24, 2, -16, -20, 6, 22, -22,        // Rank 6
      -17, -20, -12, -27, -30, -25, -14, -36, // Rank 5
      -49, -1, -27, -39, -46, -44, -33, -51,  // Rank 4
      -14, -14, -22, -46, -44, -30, -15, -27, // Rank 3
      1, 7, -8, -64, -43, -16, 9, 8,          // Rank 2
      -15, 36, 12, -54, 8, -28, 24, 14        // Rank 1
    )
  )

  inline def value(piece: Piece, sq: Square): Int =
    val visualIndex = 63 - sq.toInt
    val finalIndex  = if piece.isWhite then visualIndex else visualIndex ^ 56
    if finalIndex >= 0 && finalIndex < 64 then tables(piece.pieceType)(finalIndex) else 0
