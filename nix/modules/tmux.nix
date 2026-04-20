{ ... }:
{
  programs.tmux = {
    enable = true;

    # Core options expressed as Nix attributes
    baseIndex   = 1;
    escapeTime  = 0;
    keyMode     = "vi";
    mouse       = true;
    terminal    = "tmux-256color";

    # Everything that doesn't have a direct programs.tmux option.
    # .tmux-status.conf is deployed via dotfile-links.nix.
    extraConfig = ''
      # Start new windows/panes as login shells so /etc/zprofile is sourced
      # and Nix/Homebrew paths are available.
      set -g default-command "zsh -l"

      # True colour support
      set -as terminal-features ',xterm*:RGB'
      set -ga terminal-overrides ',tmux-256color:Tc,xterm*:Tc'

      # Saner splitting (mnemonic: v = vertical split → side by side panes)
      bind v split-window -h
      bind s split-window -v
      bind T choose-session

      # Vim-style pane navigation
      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R

      # Pane resizing
      bind -r C-h resize-pane -L 5
      bind -r C-j resize-pane -D 5
      bind -r C-k resize-pane -U 5
      bind -r C-l resize-pane -R 5

      # Reload config
      bind r source-file ~/.config/tmux/tmux.conf \; display "Reloaded tmux config"

      # Open man page in split
      bind m command-prompt -p "man page:" "split-window -h 'exec man %%'"
      bind M command-prompt -p "Man page:" "new-window -n 'man %1' 'exec man %1'"

      # Status bar settings
      set-option -g status-keys emacs

      # Refresh SSH vars on new sessions
      set -g update-environment "SSH_CLIENT SSH_CONNECTION SSH_TTY DISPLAY"

      # Load Nord-themed status bar config
      source-file ~/.tmux-status.conf

      # Re-evaluate status theme on attach (picks up SSH vars)
      set-hook -g client-attached 'source-file ~/.tmux-status.conf'

      # Toggle status bar visibility
      bind-key b set-option status
    '';
  };
}
