{ lib, pkgs, ... }:
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

  # macOS shows a confirmation dialog when the default browser changes, so
  # this only prompts while Finicky is not already the default.
  home.activation.setDefaultBrowser = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    if ! ${pkgs.defaultbrowser}/bin/defaultbrowser | grep -q '^\* finicky$'; then
      ${pkgs.defaultbrowser}/bin/defaultbrowser finicky || true
    fi
  '';
}
