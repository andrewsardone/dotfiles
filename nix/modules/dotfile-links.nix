{ config, ... }:
let
  # Repo must be cloned to this path. Update here and in README if you clone
  # it elsewhere.
  repoPath = "${config.home.homeDirectory}/.dotfiles";

  # Create a symlink from $HOME/<dest> → <repoPath>/<src> that points back
  # into the live repo checkout. Edits take effect immediately without a
  # rebuild.
  link = src: {
    source = config.lib.file.mkOutOfStoreSymlink "${repoPath}/${src}";
  };
in
{
  # ── Claude Code ───────────────────────────────────────────────────────
  # Runtime data (projects/, sessions/, history.jsonl, etc.) is excluded
  # via .claude/.gitignore so the whole directory can be symlinked safely.
  home.file.".claude" = link ".claude";

  # ── Automation / scripting ────────────────────────────────────────────
  home.file.".hammerspoon" = link ".hammerspoon";

  # ── Keyboard ──────────────────────────────────────────────────────────
  home.file.".config/karabiner" = link ".config/karabiner";

  # ── Editor ────────────────────────────────────────────────────────────
  # LazyVim manages its own plugins; just link the whole config dir.
  home.file.".config/nvim" = link ".config/nvim";

  # ── Local binaries ────────────────────────────────────────────────────
  # Symlink individual scripts so ~/.local/bin stays a real directory;
  # external installers (e.g. Claude) can drop files there without
  # appearing as untracked files in this repo.
  home.file.".local/bin/auto-git-repo-sync" = link ".local/bin/auto-git-repo-sync";
  home.file.".local/bin/dstamp"             = link ".local/bin/dstamp";
  home.file.".local/bin/git-cleanup"        = link ".local/bin/git-cleanup";
  home.file.".local/bin/new-kitty-window"   = link ".local/bin/new-kitty-window";
  home.file.".local/bin/toggle-desktop"     = link ".local/bin/toggle-desktop";
  home.file.".local/bin/tstamp"             = link ".local/bin/tstamp";
  home.file.".local/bin/wm"                 = link ".local/bin/wm";

  # ── Prompt ────────────────────────────────────────────────────────────
  # programs.starship is enabled in shell.nix; the actual settings live here.
  home.file.".config/starship.toml".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.config/starship.toml";

  # ── tmux status theme + scripts ──────────────────────────────────────
  # programs.tmux (tmux.nix) sources ~/.tmux-status.conf at runtime.
  home.file.".tmux-status.conf".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.tmux-status.conf";

  home.file.".tmux/scripts" = link ".tmux/scripts";

  # ── Git helpers ───────────────────────────────────────────────────────
  # programs.git (git.nix) references ~/.gitconfig.amazon via includeIf.
  home.file.".gitconfig.amazon".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.gitconfig.amazon";

  home.file.".gitexcludes".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.gitexcludes";

  home.file.".gitattributes".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.gitattributes";

  home.file.".githelpers".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.githelpers";

  # ── Zsh helpers ───────────────────────────────────────────────────────
  # Shell include files sourced by programs.zsh initExtra (PATH, aliases,
  # docker helpers, etc.)
  home.file.".sh-include" = link ".sh-include";

  # Custom zsh functions and completions (_boom, _brew, prompt_pure_setup,
  # etc.) added to fpath in programs.zsh initExtra.
  home.file.".zfunctions" = link ".zfunctions";

  # ── Misc dotfiles ─────────────────────────────────────────────────────
  home.file.".tigrc".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.tigrc";

  home.file.".ideavimrc".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.ideavimrc";

  home.file.".ignore".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.ignore";

  home.file.".npmrc".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.npmrc";

  home.file.".hushlogin".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.hushlogin";

  home.file.".screenrc".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.screenrc";

  home.file.".sqliterc".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.sqliterc";

  home.file.".lldbinit".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.lldbinit";

  home.file.".asdfrc".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.asdfrc";

  home.file.".gemrc".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.gemrc";

  home.file.".irbrc".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/.irbrc";

  # Makefile at $HOME for convenience tasks (cleanup, etc.)
  home.file."Makefile".source =
    config.lib.file.mkOutOfStoreSymlink "${repoPath}/Makefile";

  # ── NOT linked (managed by programs.* modules) ────────────────────────
  # .gitconfig     → programs.git generates ~/.config/git/config
  # .zshrc         → programs.zsh generates ~/.zshrc
  # .tmux.conf     → programs.tmux generates ~/.config/tmux/tmux.conf
  # .config/starship.toml is linked above; programs.starship init is
  #   injected by programs.starship.enable without overwriting the file.

  # ── NOT linked (legacy, inactive) ────────────────────────────────────
  # .vim/, .vimrc, .gvimrc     — legacy vim (still in repo for reference)
  # .emacs.d/                  — legacy emacs
  # .config/fish/              — trialled fish, reverted to zsh; inactive
  # .bashrc, .bash_profile, .bash_prompt — legacy bash
  # .iterm/                    — legacy iTerm config
  # .yabairc, .skhdrc          — replaced by AeroSpace + Karabiner
  # vendor/                    — third-party vendored files
}
