#!/usr/bin/env sh

#!/bin/bash

# === Configuration ===
DEPTH=$1
if [ -z "$DEPTH" ]; then
  echo "Usage: ./perft.sh <depth> [mode] [--fen <fen>]"
  echo "  mode: perft | divide (default: perft)"
  echo "  --fen: override the default starting position"
  exit 1
fi

MODE="perft"
FEN="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

shift
while [ $# -gt 0 ]; do
  case "$1" in
    perft|divide)
      MODE="$1"
      ;;
    --fen=*)
      FEN="${1#*=}"
      ;;
    --fen)
      shift
      if [ -z "$1" ]; then
        echo "Error: --fen requires a value"
        exit 1
      fi
      FEN="$1"
      ;;
    *)
      echo "Unknown argument: $1"
      exit 1
      ;;
  esac
  shift
done

echo "Running perft at depth $DEPTH in mode $MODE"
echo "FEN: $FEN"

# === Time the run ===
start=$(date +%s.%N)

# Run your Scala main class (replace with correct classpath/module if needed)
sbt "runMain com.ffb.zugzwang.tools.PerftRunner --fen \"$FEN\" --depth $DEPTH --mode $MODE"

end=$(date +%s.%N)
duration=$(echo "$end - $start" | bc)

echo "Elapsed time: $duration seconds"
