#!/usr/bin/env sh

#!/bin/bash

# === Configuration ===
DEPTH=$1
if [ -z "$DEPTH" ]; then
  echo "Usage: ./perft.sh <depth>"
  exit 1
fi

# Optional: override default FEN
FEN="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

echo "Running perft at depth $DEPTH"
echo "FEN: $FEN"

# === Time the run ===
start=$(date +%s.%N)

# Run your Scala main class (replace with correct classpath/module if needed)
sbt "runMain com.ffb.zugzwang.tools.PerftRunner --fen \"$FEN\" --depth $DEPTH"

end=$(date +%s.%N)
duration=$(echo "$end - $start" | bc)

echo "Elapsed time: $duration seconds"
