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
    silver-searcher  # the_silver_searcher

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
    neofetch
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
    lsd
  ];
}
