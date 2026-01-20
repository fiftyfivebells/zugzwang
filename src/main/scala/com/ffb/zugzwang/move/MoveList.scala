package com.ffb.zugzwang.move

// this is NOT immutable. for efficiency's sake, i made this basically
// just a safe wrapper around Array[Move] rather than doing something more
// "functional" and maintaining immutability
final case class MoveList(initialCapacity: Int = 256):
  private var moves: Array[Move] = new Array[Move](initialCapacity)
  private var count              = 0

  def add(move: Move): Unit =
    if count >= moves.size then grow

    moves(count) = move
    count += 1

  private def grow: Unit =
    val newMoves = new Array[Move](moves.size * 2)
    Array.copy(moves, 0, newMoves, 0, moves.length)
    moves = newMoves

  def size: Int = count

  def apply(index: Int): Option[Move] =
    if index >= count then None else Some(moves(index))

  def clear: Unit = count = 0

  def toList: List[Move] =
    (0 until count).toList.map(moves(_))

  def foreach(f: Move => Unit): Unit =
    var i = 0
    while i < count do
      f(moves(i))
      i += 1

  // this method walks through the array of moves from left and right
  // simultaneously. when it finds a move on the leftthat's not a capture,
  // it swaps it with the move on the right and checks again. This should
  // be O(n) time, since it just walks the list from boths ends
  def sortCapturesFirst: Unit =
    var left  = 0
    var right = count - 1

    while left < right do
      val leftMove = moves(left)
      if leftMove.isCapture then left += 1
      else
        val rightMove = moves(right)
        moves(right) = leftMove
        moves(left) = rightMove
        right -= 1

end MoveList
