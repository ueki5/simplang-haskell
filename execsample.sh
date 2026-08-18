mkdir -p out

for src in sample/src*.sl; do
    name=$(basename "$src" .sl)
    simplang-haskell "$src" -o "out/${name}" -S "out/${name}.s"
done
