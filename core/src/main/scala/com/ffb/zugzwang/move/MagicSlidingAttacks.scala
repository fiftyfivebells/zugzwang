package com.ffb.zugzwang.move

import com.ffb.zugzwang.board.Bitboard
import com.ffb.zugzwang.chess.Square
import com.ffb.zugzwang.tools.MagicNumberGenerator

// Bishop magic numbers
val BishopMagics: Array[Long] = Array(
  0x0010100208004010L, 0x112053040100d240L, 0x0024012202000012L, 0x0004040090002190L, 0x80020a1022800000L, 0x8401092030601020L,
  0x9004809008200001L, 0x0c00216402084000L, 0x00004828501430c0L, 0x2804481001808900L, 0x0000100086024002L, 0x01000c0410801200L,
  0x2840045040a45012L, 0x600002020b200200L, 0x05000041501010d0L, 0x0000020301084280L, 0x106010400c040082L, 0x0008101490040064L,
  0x0004080808002008L, 0x968080a802004020L, 0x1808800400a0800cL, 0x3102000188040a84L, 0x1800800148041001L, 0x0010610482051008L,
  0xc002100108101040L, 0x04081240ac500200L, 0x0000821410002200L, 0x0008082008020020L, 0x8085010113104001L, 0x0209220005018180L,
  0x0202820059084220L, 0x0002282000808800L, 0x000820040b0808c1L, 0x18019c2020100200L, 0x2001080100280840L, 0x0012100860040400L,
  0x0008030040140042L, 0x4320048080a30044L, 0x0008089c00158224L, 0x040208820082220cL, 0x1000901090000910L, 0x0804031450208201L,
  0x0900086288041010L, 0x004000a038000100L, 0x0500300202000091L, 0x0201020082020104L, 0x8020020281000202L, 0x2206020401008164L,
  0x0003410410400001L, 0x0245041084048000L, 0x80000a0184140104L, 0x00000001a0881041L, 0x0000000897040240L, 0x528248a004042000L,
  0x0030040108022404L, 0x000c104400408029L, 0x0000410410120200L, 0x0000c04c04033804L, 0x8100000100b80400L, 0x0040000885040900L,
  0x48000e1430060200L, 0x001000d00e103100L, 0x80000a2041020200L, 0x0424011004028880L
)

// Rook magic numbers
val RookMagics: Array[Long] = Array(
  0x0080004004201080L, 0x00c000c0a0001000L, 0x0200120048402080L, 0x2180041000805800L, 0x0200020010284420L, 0x9200100802000405L,
  0x0200008200042328L, 0x0900002200924100L, 0x6105800080400220L, 0x100a004101220088L, 0x4004801000200083L, 0x0202002200108842L,
  0x400a000c68220050L, 0x4100800400020080L, 0x0001010004020001L, 0x9006800041002080L, 0x0080104000200040L, 0x4541808020004000L,
  0x2000410011002002L, 0x1000808010001804L, 0x0401808004000800L, 0x8014008002008014L, 0x0000840008100221L, 0x10048a0004008741L,
  0x1101400080008020L, 0x8640200080400280L, 0x4180200080801000L, 0x00000c2100100100L, 0x00b04c0080080080L, 0x0002008200080410L,
  0x0101000100020044L, 0x0401040200184081L, 0x6240002080800053L, 0x4418d08102002200L, 0x4082200980801000L, 0x2021820802803000L,
  0x0008010031001c08L, 0x1242001004040020L, 0x8000020864005001L, 0x0008800640800900L, 0x1020842040008000L, 0x0040063008012000L,
  0x80a1002006510040L, 0x4010000800808010L, 0x0d0050080101000cL, 0x0504000200808004L, 0x01c40e2108040090L, 0x808008884b060004L,
  0x2002004028810a00L, 0x0080804000600280L, 0x2100802000100080L, 0x8201001000210900L, 0x0444008028000480L, 0x00000a00801c0080L,
  0x0491010210081400L, 0x9801018041140200L, 0x00010890a0c08001L, 0x1000420300a08092L, 0x0100d20048208142L, 0x0101201000050009L,
  0x1012000810142002L, 0x0887000224000811L, 0x0080a2102481080cL, 0x1008404400802506L
)

case class MagicEntry(
  mask: Long,
  magic: Long,
  shift: Int,
  attacks: Array[Long]
)

object MagicSlidingAttacks extends SlidingAttackGen:
  private var bishopMagics: Array[MagicEntry] = new Array[MagicEntry](64)
  private var rookMagics: Array[MagicEntry]   = new Array[MagicEntry](64)

  // get the magic numbers populated
  initialize()

  def bishopAttacks(
    square: Square,
    occupied: Bitboard
  ): Bitboard =
    val magic             = bishopMagics(square.value)
    val relevantOccupancy = occupied & magic.mask
    val index             = ((relevantOccupancy.value * magic.magic) >>> magic.shift).toInt

    Bitboard(magic.attacks(index))

  def rookAttacks(
    square: Square,
    occupied: Bitboard
  ): Bitboard =
    val magic             = rookMagics(square.value)
    val relevantOccupancy = occupied & magic.mask
    val index             = ((relevantOccupancy.value * magic.magic) >>> magic.shift).toInt

    Bitboard(magic.attacks(index))

  // Initialize all magic bitboard tables
  def initializeMagicBitboards(): Unit =
    println("Initializing magic bitboards...")

    // Initialize bishop magics
    for square <- 0 until 64 do
      bishopMagics(square) = MagicNumberGenerator.createMagicEntryForSquare(
        square,
        BishopMagics(square),
        true
      )

      if square % 8 == 0 then println(s"Initialized bishop magics for rank ${square / 8}")

    // Initialize rook magics
    for square <- 0 until 64 do
      rookMagics(square) = MagicNumberGenerator.createMagicEntryForSquare(
        square,
        RookMagics(square),
        false
      )

      if square % 8 == 0 then println(s"Initialized rook magics for rank ${square / 8}")

    // Calculate total memory usage
    val rookTableSizes   = rookMagics.map(_.attacks.length).sum
    val bishopTableSizes = bishopMagics.map(_.attacks.length).sum
    val totalMemory =
      (rookTableSizes + bishopTableSizes) * 8 // 8 bytes per Long

    println(s"Magic bitboards initialized successfully!")
    println(s"Rook tables: ${rookTableSizes} entries")
    println(s"Bishop tables: ${bishopTableSizes} entries")
    println(s"Total memory usage: ${totalMemory / 1024} KB")

  // // Validation function to test the tables
  // def validateMagicTables(): Boolean = {
  //   println("Validating magic tables...")

  //   for (square <- 0 until 64) {
  //     // Test some random occupancy patterns
  //     for (_ <- 0 until 100) {
  //       val occupancy = scala.util.Random.nextLong()

  //       // Compare magic result with direct calculation
  //       val magicRookAttacks = rookAttacks(square, occupancy)
  //       val directRookAttacks = rookAttacksFromOccupancy(square, occupancy)

  //       if (magicRookAttacks != directRookAttacks) {
  //         println(s"Rook validation failed for square $square")
  //         return false
  //       }

  //       val magicBishopAttacks = bishopAttacks(square, occupancy)
  //       val directBishopAttacks = bishopAttacksFromOccupancy(square, occupancy)

  //       if (magicBishopAttacks != directBishopAttacks) {
  //         println(s"Bishop validation failed for square $square")
  //         return false
  //       }
  //     }
  //   }

  //   println("All magic tables validated successfully!")
  //   true
  // }

  // Call this once when your engine starts up
  def initialize(): Unit =
    initializeMagicBitboards()
  // if (!validateMagicTables()) {
  //   throw new RuntimeException("Magic table validation failed!")
  // }
