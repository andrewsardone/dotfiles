if command -v ncdu >/dev/null 2>&1; then
  alias ncdu="ncdu --color dark"
fi

if command -v bat >/dev/null 2>&1; then
  alias cat="bat --paging=never"
fi

if type /usr/local/bin/vim > /dev/null 2>&1; then
  alias vi='/usr/local/bin/vim'
fi
if type nvim > /dev/null 2>&1; then
  alias vim='nvim'
  alias vi='nvim'
  alias view='nvim -R'
fi

alias claude-yolo="claude --dangerously-skip-permissions"

if command -v opencode >/dev/null 2>&1; then
  opencode() {
    OPENCODE_CONFIG_CONTENT='{"plugin":[]}' command opencode "$@"
  }
  omo() {
    command opencode "$@"
  }
fi
