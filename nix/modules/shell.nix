{ config, pkgs, ... }:
{
  # ── Fish ──────────────────────────────────────────────────────────────
  programs.fish = {
    enable = true;

    # Runs for all fish shells (login + interactive). Good for PATH and
    # universal exports that must be set early.
    loginShellInit = ''
      set -gx PATH /opt/homebrew/sbin $PATH
      set -gx PATH /opt/homebrew/bin $PATH
      set -gx PATH /nix/var/nix/profiles/default/bin $PATH
      set -gx PATH ~/.nix-profile/bin $PATH
      set -gx PATH ~/.toolbox/bin $PATH
      set -gx PATH ~/bin $PATH
      set -gx PATH ~/.local/bin $PATH
      # Adds relative ./bin to PATH — useful in project roots with local scripts
      set -gx PATH bin $PATH
      set -gx PATH "$HOME/.aim/mcp-servers" $PATH
    '';

    # Runs only for interactive shells.
    interactiveShellInit = ''
      set fish_greeting ""

      # Environment
      set -gx EDITOR nvim
      set -gx TERM xterm-256color

      # Vi key bindings
      set -g fish_key_bindings fish_vi_key_bindings

      # Theme (requires fish-gruvbox plugin, managed via fish_plugins + fisher)
      if functions -q theme_gruvbox
        theme_gruvbox "dark" "hard"
      end
      set -U fish_color_command c397d8
      set -U fish_color_autosuggestion 969896

      # Prompt — starship init is injected by programs.starship below;
      # this guard is kept for machines where starship isn't available.
      if not command -qv starship
        # fallback: no custom prompt
      end

      # ── Smart ls ──────────────────────────────────────────────────
      function aps_smart_ls
        clear
        pwd
        if test (ls -a $argv | wc -l) -lt 40
          ll $argv
        else
          l $argv
        end
      end
      alias sl aps_smart_ls

      # ── Navigation ────────────────────────────────────────────────
      function aps_pushd
        pushd $argv[1] && aps_smart_ls
      end
      alias f aps_pushd

      function aps_popd
        popd && aps_smart_ls
      end
      alias d aps_popd
    '';

    shellAliases = {
      l    = "ls -a";
      ll   = "ls -la";
      vi   = "nvim";
      vim  = "nvim";
      view = "nvim -R";
      # bat replaces cat when available
      cat  = "bat --paging=never";
      # ncdu with colour
      ncdu = "ncdu --color dark";
      # bottom compact mode
      btm  = "btm -b --mem_as_value";
      # lsd replaces ls when available
      ls   = "lsd";
      # opencode wrappers (matches .sh-include/aliases.sh)
      claude-yolo = "claude --dangerously-skip-permissions";
    };
  };

  # fish_plugins file is deployed via dotfile-links.nix so fisher can manage
  # the gruvbox theme and other plugins. Run `fisher update` after first setup.

  # ── Starship ──────────────────────────────────────────────────────────
  # Enables shell integration (adds `starship init fish | source` to fish).
  # The actual configuration lives in .config/starship.toml, deployed via
  # dotfile-links.nix.
  # enable = true adds `starship init fish | source` to the fish config.
  # The actual starship.toml lives in the repo and is symlinked by
  # dotfile-links.nix. No settings are declared here to avoid home-manager
  # writing its own ~/.config/starship.toml and conflicting with the link.
  programs.starship.enable = true;
}
