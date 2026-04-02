#!/usr/bin/env bash
if ! command -v fzf &>/dev/null; then
  tmux choose-tree -Zs
  exit 0
fi

FOCUS_LOG="$HOME/.tmux/session-focus.log"

# Build an associative lookup of focus timestamps
declare -A focus_ts
if [ -f "$FOCUS_LOG" ]; then
  while IFS=$'\t' read -r ts key; do
    focus_ts[$key]=$ts
  done < "$FOCUS_LOG"
fi

# Annotate each session with its last-focus timestamp, sort, strip timestamp
sorted=$(
  while IFS=$'\t' read -r key rest; do
    printf '%s\t%s %s\n' "${focus_ts[$key]:-0}" "$key" "$rest"
  done < <(tmux list-sessions -F '#{session_name}'$'\t''(#{session_windows} windows)') \
  | sort -rn | cut -f2-
)

selected=$(echo "$sorted" | \
  fzf \
    --height 100% \
    --no-sort \
    --border-label ' sessions ' \
    --prompt '📂 ' \
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
