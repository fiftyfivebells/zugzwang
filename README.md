# zugzwang

![Scala Version](https://img.shields.io/badge/Scala-3.3.1-blue)
![Status](https://img.shields.io/badge/status-WIP-orange)

> *Where every move is legal, but none are good.*

**`zugzwang`** is a purely functional chess logic library written in Scala 3. It models the full state of a chess game using immutable data structures and bitboards, enforcing the rules of classical chess with clarity and precision.

Designed for composability and correctness, `zugzwang` provides a foundation for building chess applications, bots, analysis tools, or anything else that needs a reliable and deterministic source of truth for chess logic.

It won’t help you become a better player, but it will make sure your mistakes are legal.

---

## Table of Contents

- [Features](#features)
- [Getting Started](#getting-started)
  - [Installation](#installation)
  - [Usage Example](#usage-example)
- [Modules and Structure](#modules-and-structure)
- [API Documentation](#api-documentation)
- [Why the Name?](#why-the-name)

---

## Features

- ♟️ Immutable, bitboard-based representation of chess state
- 🧠 Pure rule enforcement: legal move generation, castling, promotion, en passant, draw detection
- 📦 Scala 3 idioms: enums, opaque types, case classes, exhaustiveness checks
- 🛠️ Functional API designed for predictability and testability
- 🌐 Ready for use in UIs, engines, or education projects

---

## Getting Started

### Installation

More to come...

## Features

- Immutable, bitboard-based representation of game state
- Full rule enforcement: legal move generation, castling, promotion, en passant, draw detection, and more
- Pure functional API with referential transparency at every layer
- Built with Scala 3 idioms: enums, opaque types, case classes, and exhaustiveness checks
- Designed to be embedded in web apps, engines, or educational tools

---

<!-- ## Example -->

<!-- ```scala -->
<!-- import zugzwang.core._ -->

<!-- val game = Game.initial -->
<!-- val move = Move(Square.E2, Square.E4) -->

<!-- game.play(move) match -->
<!--   case Right(nextGame) => println("Move applied.") -->
<!--   case Left(error)     => println(s"Illegal move: $error") -->

<!-- ## Modules and Structure -->

<!-- `zugzwang` is organized into several modules for clarity, modularity, and testability: -->

<!-- ### `zugzwang.core` -->
<!-- - The heart of the library. -->
<!-- - Contains immutable data structures for board state, pieces, and moves. -->
<!-- - Provides pure functions for applying moves and advancing game state. -->

<!-- ### `zugzwang.rules` -->
<!-- - Encodes the rules of chess. -->
<!-- - Validates legal moves, castling conditions, check detection, and special moves like en passant. -->

<!-- ### `zugzwang.fen` -->
<!-- - Parses and serializes FEN (Forsyth–Edwards Notation). -->
<!-- - Allows saving and restoring full game state from strings. -->

<!-- ### `zugzwang.utils` -->
<!-- - Bitboard utilities, square conversions, file/rank constants, and helpful combinators. -->

<!-- ### `zugzwang.testkit` -->
<!-- - Provides test helpers, board setup shortcuts, and assertions for writing game state tests. -->

<!-- > Each module is fully immutable, side-effect free, and designed with Scala 3 idioms like enums, opaque types, and exhaustive matching. -->


## Why the name?

In chess, zugzwang refers to a situation where a player is forced to make a move even though every legal option weakens their position.

In software, and especially in functional programming, that can feel very familiar.
