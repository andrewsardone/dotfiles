#!/usr/bin/env bash
if ! command -v fzf &>/dev/null; then
  tmux choose-tree -Zw
  exit 0
fi

selected=$(tmux list-windows -a -F '#{session_name}:#{window_index} #{window_name} #{pane_current_path}' | \
  fzf \
    --height 100% \
    --no-sort \
    --border-label ' windows ' \
    --prompt '🪟 ' \
    --preview 'tmux capture-pane -ep -t {1}' \
    --preview-window 'down:80%:wrap')

if [ -n "$selected" ]; then
  target=$(echo "$selected" | cut -d' ' -f1)
  tmux switch-client -t "$target"
fi
