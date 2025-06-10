package com.ffb.zugzwang.chess

enum Color:
  case White, Black

  def enemy: Color = this match {
    case White => Black
    case Black => White
  }

  def toFen: String = this match {
    case White => "w"
    case Black => "b"
  }

end Color
