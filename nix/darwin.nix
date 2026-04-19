{ pkgs, ... }:
{
  imports = [
    ./modules/homebrew.nix
    ./modules/macos-defaults.nix
    ./modules/packages.nix
  ];

  # .config/nix/nix.conf already sets experimental-features; declaring here
  # too so the flake-managed system is self-contained.
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  nixpkgs.hostPlatform = "aarch64-darwin";

  # Required: matches the nix-darwin major version used at first install.
  system.stateVersion = 5;

  system.primaryUser = "andrew";

  users.users.andrew = {
    name = "andrew";
    home = "/Users/andrew";
  };
}
