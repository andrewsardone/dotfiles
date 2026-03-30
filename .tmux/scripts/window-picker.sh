#!/usr/bin/env bash
if ! command -v fzf &>/dev/null; then
  tmux choose-tree -Zw
  exit 0
fi

FOCUS_LOG="$HOME/.tmux/window-focus.log"

# Build an associative lookup of focus timestamps
declare -A focus_ts
if [ -f "$FOCUS_LOG" ]; then
  while IFS=$'\t' read -r ts key; do
    focus_ts[$key]=$ts
  done < "$FOCUS_LOG"
fi

# Annotate each window with its last-focus timestamp, sort, strip timestamp
# Format after cut: "session:window_index window_name pane_current_path"
sorted=$(
  while IFS=$'\t' read -r key rest; do
    printf '%s\t%s %s\n' "${focus_ts[$key]:-0}" "$key" "$rest"
  done < <(tmux list-windows -a -F '#{session_name}:#{window_index}'$'\t''#{window_name} #{pane_current_path}') \
  | sort -rn | cut -f2-
)

selected=$(echo "$sorted" | \
  fzf \
    --height 100% \
    --no-sort \
    --border-label ' windows ' \
    --prompt '🪟 ' \
    --preview 'tmux capture-pane -ep -t {1}' \
    --preview-window 'down:80%:wrap')

if [ -n "$selected" ]; then
  target=$(echo "$selected" | cut -d' ' -f1)
  if [ -n "$TMUX" ]; then
    tmux switch-client -t "$target"
  else
    tmux attach-session -t "$target"
  fi
fi
