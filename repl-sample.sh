#!/usr/bin/env bash
# 使い方: ./repl-sample.sh samp002   （省略時は samp001）
base=${1:-samp001}

if [ ! -f "sample/${base}.sl" ]; then
  echo "sample/${base}.sl が見つかりません" >&2
  exit 1
fi
mkdir -p output

# :set args だけを書いた一時スクリプトを作る
tmp=$(mktemp --suffix=.ghci)
trap 'rm -f "$tmp"' EXIT
cat >"$tmp" <<EOF
:set args sample/${base}.sl -o output/${base} -S output/${base}.s
EOF

cabal repl lib:simplang-haskell \
  -b optparse-applicative,directory,process \
  --repl-no-load \
  --repl-options="-ghci-script=sample/repl.ghci -ghci-script=$tmp"
