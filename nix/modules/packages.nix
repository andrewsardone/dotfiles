{ pkgs, ... }:
{
  # CLI tools sourced from nixpkgs-unstable.
  # Tools managed by programs.* modules (git, zsh, starship, tmux) are
  # excluded here to avoid duplication.
  # Tools only available via Homebrew taps or macOS-specific are in
  # homebrew.nix instead.
  environment.systemPackages = with pkgs; [
    # Search / navigation
    ripgrep
    fd
    bat
    fzf
    lsd
    ack
    yazi
    # TODO: verify nixpkgs attr name; may be `silver-searcher` or removed
    # silver-searcher  # the_silver_searcher (ag)

    # File / disk utilities
    ncdu
    tree
    sqlite

    # Build / dev tools
    actionlint
    ast-grep
    shellcheck
    shfmt
    gnupg
    uv
    gh       # github-cli
    lazygit
    tig

    # Media / web
    wget

    # System monitoring
    bottom   # btm

    # Editors / coding
    neovim

    # Misc
    jq
    fastfetch
  ];
}
