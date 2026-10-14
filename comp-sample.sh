#!/usr/bin/env bash
mkdir -p output
cabal install exe:simplang-haskell --overwrite-policy=always

for samp in sample/samp*.sl; do # samp001.sl
  base=$(basename "$samp" .sl)  # samp001

  echo "compiling sample source ... simplang-haskell ${samp} -o output/${base} -d output"
  simplang-haskell "$samp" -o "output/${base}" -d output
done
