{ pkgs, ... }:
{
  # CLI tools sourced from nixpkgs-unstable.
  # Tools managed by programs.* modules (git, fish, starship, tmux) are
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
    # TODO: verify nixpkgs attr name; may be `silver-searcher` or removed
    # silver-searcher  # the_silver_searcher (ag)

    # File / disk utilities
    ncdu
    tree
    stow
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
    entr

    # Media / web
    ffmpeg
    mpv
    gifsicle
    wget
    hugo

    # System monitoring
    bottom   # btm
    htop
    watch

    # Network / infra
    socat
    tmate
    restic

    # Editors / coding
    neovim
    vim

    # Language tooling
    vale

    # Cloud
    awscli2

    # Misc
    jq
    ranger
    tig
    # TODO: neofetch is unmaintained; may be removed from nixpkgs.
    # Replace with fastfetch if missing: https://github.com/fastfetch-cli/fastfetch
    neofetch
  ];
}
