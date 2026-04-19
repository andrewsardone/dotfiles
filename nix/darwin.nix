{ pkgs, ... }:
{
  imports = [
    ./modules/homebrew.nix
    ./modules/macos-defaults.nix
    ./modules/packages.nix
  ];

  # Determinate Nix manages its own daemon; disable nix-darwin's Nix management
  # to avoid conflicts. This means nix.* settings options are unavailable, but
  # Determinate handles experimental-features and upgrades independently.
  nix.enable = false;

  nixpkgs.hostPlatform = "aarch64-darwin";

  # Required: matches the nix-darwin major version used at first install.
  system.stateVersion = 5;

  system.primaryUser = "andrew";

  users.users.andrew = {
    name = "andrew";
    home = "/Users/andrew";
  };
}
