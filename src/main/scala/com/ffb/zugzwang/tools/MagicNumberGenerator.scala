package com.ffb.zugzwang.tools

import com.ffb.zugzwang.move.MagicEntry

import scala.collection.mutable.ArrayBuffer
import scala.util.{Random, boundary}

object MagicNumberGenerator:

  // Generate occupancy mask for rook (fancy magic - excludes edges)
  def rookMask(square: Int): Long =
    val rank = square / 8
    val file = square % 8
    var mask = 0L

    // Horizontal (exclude file 0 and 7)
    for f <- 1 until 7 if f != file do mask |= 1L << (rank * 8 + f)

    // Vertical (exclude rank 0 and 7)
    for r <- 1 until 7 if r != rank do mask |= 1L << (r * 8 + file)

    mask

  // Generate occupancy mask for bishop (fancy magic - excludes edges)
  def bishopMask(square: Int): Long =
    val rank = square / 8
    val file = square % 8
    var mask = 0L

    val directions = Array((-1, -1), (-1, 1), (1, -1), (1, 1))

    for (dr, df) <- directions do
      var r = rank + dr
      var f = file + df
      // Stop before hitting edges (ranks 0,7 and files 0,7)
      while r >= 1 && r <= 6 && f >= 1 && f <= 6 do
        mask |= 1L << (r * 8 + f)
        r += dr
        f += df

    mask

  // Generate rook attacks from a given occupancy (your hyperbola quintessence replacement)
  def rookAttacksFromOccupancy(square: Int, occupancy: Long): Long =
    val rank    = square / 8
    val file    = square % 8
    var attacks = 0L

    val directions = Array((0, 1), (0, -1), (1, 0), (-1, 0)) // E, W, S, N

    for (dr, df) <- directions do
      var r        = rank + dr
      var f        = file + df
      var continue = true
      while r >= 0 && r <= 7 && f >= 0 && f <= 7 && continue do
        val square = r * 8 + f
        attacks |= 1L << square
        // Stop if we hit an occupied square (but include the blocking piece)
        if (occupancy & (1L << square)) != 0 then continue = false
        else
          r += dr
          f += df

    attacks

  // Generate bishop attacks from a given occupancy
  def bishopAttacksFromOccupancy(square: Int, occupancy: Long): Long =
    val rank    = square / 8
    val file    = square % 8
    var attacks = 0L

    val directions = Array((-1, -1), (-1, 1), (1, -1), (1, 1)) // NW, NE, SW, SE

    for (dr, df) <- directions do
      var r        = rank + dr
      var f        = file + df
      var continue = true
      while r >= 0 && r <= 7 && f >= 0 && f <= 7 && continue do
        val square = r * 8 + f
        attacks |= 1L << square
        // Stop if we hit an occupied square (but include the blocking piece)
        if (occupancy & (1L << square)) != 0 then continue = false
        else
          r += dr
          f += df

    attacks

  // Extract bit positions from a mask
  def extractBitPositions(mask: Long): Array[Int] =
    val positions = ArrayBuffer[Int]()
    var temp      = mask
    while temp != 0 do
      val bit = java.lang.Long.numberOfTrailingZeros(temp)
      positions += bit
      temp &= temp - 1 // Clear lowest set bit
    positions.toArray

  // Generate all possible occupancy patterns for a given mask
  def generateAllOccupancies(mask: Long): Array[Long] =
    val bits  = extractBitPositions(mask)
    val count = 1 << bits.length

    (0 until count).map { i =>
      var occupancy = 0L
      var tempI     = i
      for bit <- bits do
        if (tempI & 1) != 0 then occupancy |= (1L << bit)
        tempI >>= 1
      occupancy
    }.toArray

  // Generate a candidate magic number (sparse bit pattern)
  def randomMagicCandidate(): Long =
    Random.nextLong() & Random.nextLong() & Random.nextLong()

  // Test if a magic number works for the given occupancies and attacks
  def testMagic(
    magic: Long,
    mask: Long, // Note: 'mask' is not used in the current function body
    occupancies: Array[Long],
    attacks: Array[Long],
    bits: Int
  ): Boolean =
    // The 'boundary' block defines the scope from which 'break' will exit.
    // The type parameter [Boolean] indicates that 'break' will return a Boolean,
    // and thus the 'boundary' expression itself will evaluate to a Boolean.
    boundary[Boolean] {
      val shift     = 64 - bits
      val tableSize = 1 << bits // Equivalent to 2^bits
      val table     = new Array[Long](tableSize)

      for i <- occupancies.indices do
        // The original calculation for the index remains the same.
        // Ensure that occupancies(i) & mask is used if mask is intended for filtering bits of occupancy
        // For magic bitboards, typically it's (occupancy & mask) * magic >>> shift
        // Assuming occupancies(i) already represents the relevant subset of blockers.
        val index = ((occupancies(i) * magic) >>> shift).toInt

        if table(index) == 0L then table(index) = attacks(i)
        else if table(index) != attacks(i) then
          // Instead of 'return false', we 'break' out of the 'boundary'
          // with the value 'false'.
          boundary.break(false) // Hash collision with different attack patterns

      // If the loop completes without any 'break(false)',
      // it means the magic number is valid for the given sets.
      // This 'true' becomes the return value of the 'boundary' block.
      true
    }

  // Find a magic number for a specific square
  def findMagicNumber(square: Int, isRook: Boolean): Long =
    val mask        = if isRook then rookMask(square) else bishopMask(square)
    val bits        = java.lang.Long.bitCount(mask)
    val occupancies = generateAllOccupancies(mask)
    val attacks = occupancies.map(occ =>
      if isRook then rookAttacksFromOccupancy(square, occ)
      else bishopAttacksFromOccupancy(square, occ)
    )

    println(
      s"Finding magic for ${if isRook then "rook" else "bishop"} on square $square (${bits} bits, ${occupancies.length} occupancies)"
    )

    var attempts    = 0
    val maxAttempts = 100_000_000

    while attempts < maxAttempts do
      val magic = randomMagicCandidate()
      if testMagic(magic, mask, occupancies, attacks, bits) then
        println(
          s"Found magic for square $square after $attempts attempts: 0x${magic.toHexString.toUpperCase}L"
        )
        return magic
      attempts += 1

      if attempts % 1_000_000 == 0 then println(s"  ... attempted ${attempts / 1_000_000}M combinations")

    throw new RuntimeException(
      s"Failed to find magic for square $square after $maxAttempts attempts"
    )

  // Generate all magic numbers
  def generateAllMagicNumbers(): (Array[Long], Array[Long]) =
    println("Generating rook magic numbers...")
    val rookMagics = (0 until 64).map(sq => findMagicNumber(sq, true)).toArray

    println("\nGenerating bishop magic numbers...")
    val bishopMagics =
      (0 until 64).map(sq => findMagicNumber(sq, false)).toArray

    (rookMagics, bishopMagics)

  // Print magic numbers in a format ready for code
  def printMagicNumbers(
    rookMagics: Array[Long],
    bishopMagics: Array[Long]
  ): Unit =
    println("\n// Rook magic numbers")
    println("val ROOK_MAGICS: Array[Long] = Array(")
    rookMagics.zipWithIndex.foreach { case (magic, i) =>
      val suffix = if i == 63 then "L" else "L,"
      println(f"  0x${magic}%016XL$suffix%s // Square $i%2d")
    }
    println(")")

    println("\n// Bishop magic numbers")
    println("val BISHOP_MAGICS: Array[Long] = Array(")
    bishopMagics.zipWithIndex.foreach { case (magic, i) =>
      val suffix = if i == 63 then "L" else "L,"
      println(f"  0x${magic}%016XL$suffix%s // Square $i%2d")
    }
    println(")")

  def createMagicEntryForSquare(
    square: Int,
    magicNumber: Long,
    isBishop: Boolean
  ): MagicEntry =
    val mask        = if isBishop then bishopMask(square) else rookMask(square)
    val bits        = java.lang.Long.bitCount(mask)
    val shift       = 64 - bits
    val attackTable = buildAttackTable(square, mask, magicNumber, isBishop)

    MagicEntry(mask, magicNumber, shift, attackTable)

  // Build attack table for a specific square
  private def buildAttackTable(
    square: Int,
    mask: Long,
    magic: Long,
    isBishop: Boolean
  ): Array[Long] =
    //  val mask = if (isRook) rookMask(square) else bishopMask(square)
    //  val magic = if (isRook) RookMagics(square) else BishopMagics(square)
    val bits      = java.lang.Long.bitCount(mask)
    val shift     = 64 - bits
    val tableSize = 1 << bits

    // Create the attack table
    val attackTable = new Array[Long](tableSize)

    // Generate all possible occupancy patterns for this square
    val occupancies = generateAllOccupancies(mask)

    // For each occupancy pattern, calculate attacks and store in table
    for occupancy <- occupancies do
      val attacks =
        if isBishop then bishopAttacksFromOccupancy(square, occupancy)
        else rookAttacksFromOccupancy(square, occupancy)

      // Calculate the index using the magic number
      val index = ((occupancy * magic) >>> shift).toInt

      // Store the attacks at this index
      attackTable(index) = attacks

    attackTable

  def main(args: Array[String]): Unit =
    val (rookMagics, bishopMagics) = generateAllMagicNumbers()
    printMagicNumbers(rookMagics, bishopMagics)
