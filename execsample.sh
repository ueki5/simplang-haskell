mkdir -p output
cabal install exe:simplang-haskell --overwrite-policy=always

for src in sample/src*.sl; do 
    base=$(basename "$src" .sl)        # src00x
    name="out${base#src}"             # out00x (先頭にoutを付加し、"${base#src}" でsrcを除去)
    simplang-haskell "$src" -o "output/${name}" -S "output/${name}.s"
done
