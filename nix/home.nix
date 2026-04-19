{ config, pkgs, ... }:
let
  # Repo must be cloned to this path. Update here and in README if you clone
  # it elsewhere.
  repoPath = "${config.home.homeDirectory}/dotfiles";
in
{
  imports = [
    ./modules/git.nix
    ./modules/shell.nix
    ./modules/tmux.nix
    ./modules/dotfile-links.nix
  ];

  _module.args = { inherit repoPath; };

  home.username = "andrew";
  home.homeDirectory = "/Users/andrew";

  # Keep in sync with nixpkgs input; update after `nix flake update`.
  home.stateVersion = "24.11";

  programs.home-manager.enable = true;
}
