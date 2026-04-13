#!/usr/bin/env bash
if ! command -v fzf &>/dev/null; then
  tmux choose-tree -Zs
  exit 0
fi

# Use tmux's native session_activity for MRU ordering — no log files needed
sorted=$(
  tmux list-sessions -F "#{session_activity}"$'\t'"#{session_name}"$'\t'"(#{session_windows} windows)" \
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
  target=$(echo "$selected" | cut -f1)
  if [ -n "$TMUX" ]; then
    tmux switch-client -t "$target"
  else
    tmux attach-session -t "$target"
  fi
fi
