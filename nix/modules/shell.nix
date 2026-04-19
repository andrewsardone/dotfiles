{ ... }:
{
  # ── Zsh ───────────────────────────────────────────────────────────────
  programs.zsh = {
    enable = true;

    # Start in vi insert mode (equivalent to `bindkey -v`)
    defaultKeymap = "viins";

    history = {
      size          = 100000;
      save          = 100000;
      path          = "$HOME/.zhistory";
      share         = true;   # SHARE_HISTORY + INC_APPEND_HISTORY
      ignoreAllDups = true;   # HIST_IGNORE_ALL_DUPS
    };
    # Note: APPEND_HISTORY, INC_APPEND_HISTORY, and HIST_REDUCE_BLANKS are
    # set explicitly in initExtra below (not covered by programs.zsh.history).

    # Syntax highlighting from nixpkgs — replaces the vendored
    # .zfunctions/zsh-syntax-highlighting/ submodule that was removed.
    syntaxHighlighting.enable = true;

    shellAliases = {
      # Shell
      "reload!" = "source ~/.zshrc";
      ded       = "rm -rf ~/Library/Developer/Xcode/DerivedData/";

      # Tools
      todos = "ack --nogroup '(TODO|FIX(ME)?):')";
      tp    = "touch todo.taskpaper && vi todo.taskpaper";
      jsc   = "/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc";

      # Fix `sed` locale bug in `pass`
      pass  = "LC_ALL=C pass";

      # Finder
      reset-finder-open-with-menu = "/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister -kill -r -domain local -domain system -domain user; killall Finder";

      # From .sh-include/aliases.sh
      ncdu  = "ncdu --color dark";
      cat   = "bat --paging=never";
      vi    = "nvim";
      vim   = "nvim";
      view  = "nvim -R";

      # opencode / claude wrappers
      claude-yolo = "claude --dangerously-skip-permissions";
    };

    initContent = ''
      # ── fpath ─────────────────────────────────────────────────────
      fpath=( "$HOME/.zfunctions" $fpath )

      # ── Shared shell include files ─────────────────────────────────
      # Sources .sh-include/{path.sh,aliases.sh,docker.sh,...} for PATH
      # setup, shared aliases, and other cross-shell config.
      sh_common_dir="$HOME/.sh-include"
      if [ -d "$sh_common_dir" ]; then
        for include in "$sh_common_dir"/*; do
          source "$include"
        done
      fi

      # ── Timing / input ────────────────────────────────────────────
      KEYTIMEOUT=1

      # ── Colors ────────────────────────────────────────────────────
      autoload -U colors && colors
      export LSCOLORS="exfxcxdxbxegedabagacad"
      export CLICOLOR=true

      # ── Editor ────────────────────────────────────────────────────
      export EDITOR=nvim

      # ── Cursor shape for vi mode (iTerm2 and tmux) ────────────────
      function block_cursor() { print -n '\e[2 q' }
      function line_cursor()  { print -n '\e[6 q' }

      function zle-keymap-select zle-line-init {
        case $KEYMAP in
          vicmd)      block_cursor ;;
          viins|main) line_cursor  ;;
        esac
        zle reset-prompt
        zle -R
      }

      function zle-line-finish { block_cursor }

      zle -N zle-line-init
      zle -N zle-line-finish
      zle -N zle-keymap-select

      # ── Key bindings ──────────────────────────────────────────────
      bindkey '^R' history-incremental-search-backward

      # C-x C-e: edit current command in $EDITOR (bash-style)
      autoload -U edit-command-line
      zle -N edit-command-line
      bindkey -M vicmd v edit-command-line
      bindkey '\C-x\C-e' edit-command-line

      # ── Prompt ────────────────────────────────────────────────────
      # programs.starship adds `eval "$(starship init zsh)"` automatically.
      # pure prompt is kept as a fallback for machines without starship.
      if ! command -v starship >/dev/null 2>&1; then
        autoload -U promptinit; promptinit; prompt pure
      fi
      PURE_GIT_PULL=0

      # ── History (options not covered by programs.zsh.history) ─────
      setopt APPEND_HISTORY
      setopt INC_APPEND_HISTORY
      setopt HIST_REDUCE_BLANKS

      # ── Pager ─────────────────────────────────────────────────────
      export LESS="-RIM"

      # ── hub (git → github) ────────────────────────────────────────
      command -v hub >/dev/null 2>&1 && alias git=hub

      # ── fzf ───────────────────────────────────────────────────────
      export FZF_DEFAULT_OPTS='--height 40% --reverse --border --multi'
      export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --no-ignore-vcs -g "!{node_modules,.git}"'
      [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

      # ── GPG ───────────────────────────────────────────────────────
      export GPG_TTY=$(tty)

      # ── Filesystem marks ──────────────────────────────────────────
      export MARKPATH="$HOME/.marks"
      function jump   { f "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1" }
      function mark   { mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1" }
      function unmark { rm -i "$MARKPATH/$1" }
      function marks  {
        \ls -l "$MARKPATH" | tail -n +2 | sed 's/  / /g' | cut -d' ' -f9- \
          | awk -F ' -> ' '{printf "%-10s -> %s\n", $1, $2}'
      }
      function _completemarks { reply=($(ls "$MARKPATH")) }
      compctl -K _completemarks jump
      compctl -K _completemarks unmark

      # ── Go ────────────────────────────────────────────────────────
      export GOPATH="$HOME/source/gopath"
      export PATH="$PATH:$GOPATH/bin"

      # ── Cross-terminal directory history (fzf jump) ───────────────
      DIRHISTORY_FILE="$HOME/.dir_history"
      autoload -U add-zsh-hook
      add-zsh-hook chpwd _log_dir

      _log_dir() {
        local f="$DIRHISTORY_FILE"
        { echo "$PWD"; cat "$f" 2>/dev/null } | awk '!seen[$0]++' | head -200 > "$f.tmp" \
          && mv "$f.tmp" "$f"
      }

      j() {
        local dir
        dir=$(cat "$DIRHISTORY_FILE" 2>/dev/null | fzf --no-sort) && pushd "$dir"
      }

      # ── tmux helpers ──────────────────────────────────────────────
      # t: fzf window picker (or new session if none exist)
      t() {
        if ! tmux list-sessions 2>/dev/null; then
          tmux new-session
          return
        fi
        if [ -x "$HOME/.tmux/scripts/window-picker.sh" ]; then
          "$HOME/.tmux/scripts/window-picker.sh"
        else
          tmux list-sessions
        fi
      }

      # ts: fzf session picker
      ts() {
        if ! tmux list-sessions 2>/dev/null; then
          tmux new-session
          return
        fi
        if [ -x "$HOME/.tmux/scripts/session-picker.sh" ]; then
          "$HOME/.tmux/scripts/session-picker.sh"
        else
          tmux list-sessions
        fi
      }

      # peek: open file in a 33% tmux split
      peek() { tmux split-window -p 33 "$EDITOR" "$@" || exit; }

      # ── did.txt ───────────────────────────────────────────────────
      : "''${APS_DID_FILE:=~/Dropbox/Documents/notes/did.txt}"
      alias did="vim +'normal Go' +'r!date' +'normal o' $APS_DID_FILE"

      # ── Optional / machine-specific includes ──────────────────────
      [ -f "$HOME/.zshrc.amazon" ] && source "$HOME/.zshrc.amazon"

      if [ -d ~/.vault/env ]; then
        for e in ~/.vault/env/*.env; do source "$e"; done
      fi

      # AIM CLI
      export PATH="$HOME/.aim/mcp-servers:$PATH"
    '';
  };

  # ── Starship ──────────────────────────────────────────────────────────
  # Injects `eval "$(starship init zsh)"` into the generated .zshrc.
  # The actual starship.toml lives in the repo and is symlinked by
  # dotfile-links.nix. No settings are declared here to avoid home-manager
  # writing its own ~/.config/starship.toml and conflicting with the link.
  programs.starship.enable = true;
}
