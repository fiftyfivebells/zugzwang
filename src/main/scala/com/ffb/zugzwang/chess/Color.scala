package com.ffb.zugzwang.chess

enum Color:
  case White, Black

  def enemy: Color = this match {
    case White => Black
    case Black => White
  }

end Color
