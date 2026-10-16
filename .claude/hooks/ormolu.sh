#!/usr/bin/env bash
# Claude Code の PostToolUse hook（Write・Edit）：書き込んだ .hs を ormolu で整形する（docs/step027.md）
# ormolu は devShell（flake.nix）で版を固定したものを direnv 経由で使い、エディタ・CI と整形結果を揃える
set -u

file=$(jq -r '.tool_input.file_path // empty')
case "$file" in
  *.hs) ;;
  *) exit 0 ;;
esac
[ -f "$file" ] || exit 0

project=${CLAUDE_PROJECT_DIR:-$(pwd)}
ormolu=$(direnv exec "$project" sh -c 'command -v ormolu' 2>/dev/null)
if [ -z "$ormolu" ]; then
  # 利用者に知らせるだけで、Claude の作業は止めない
  echo "ormolu hook: ormolu not found; run 'direnv allow' in $project" >&2
  exit 1
fi

# 構文エラーなどで整形できなかったときは、内容を Claude に返す
if ! err=$("$ormolu" --mode inplace "$file" 2>&1); then
  echo "ormolu hook: failed to format $file" >&2
  echo "$err" >&2
  exit 2
fi
