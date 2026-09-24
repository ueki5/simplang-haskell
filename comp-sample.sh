mkdir -p output
cabal install exe:simplang-haskell --overwrite-policy=always

for samp in sample/samp*.sl; do # samp001.sl
  base=$(basename "$samp" .sl)  # samp001
  simplang-haskell "$samp" -o "output/${base}" -S "output/${base}.s"
done
