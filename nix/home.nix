{ config, pkgs, ... }:
{
  imports = [
    ./modules/git.nix
    ./modules/shell.nix
    ./modules/tmux.nix
    ./modules/dotfile-links.nix
  ];

  # repoPath is defined in modules/dotfile-links.nix as:
  #   "${config.home.homeDirectory}/dotfiles"
  # If you clone this repo to a different location, update it there.

  home.username = "andrew";
  home.homeDirectory = "/Users/andrew";

  # Keep in sync with nixpkgs input; update after `nix flake update`.
  home.stateVersion = "24.11";

  programs.home-manager.enable = true;
}
