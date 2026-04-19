{ pkgs, ... }:
{
  imports = [
    ./modules/homebrew.nix
    ./modules/macos-defaults.nix
    ./modules/packages.nix
  ];

  # Disable slow default behaviours in the nix-darwin-generated /etc/zshrc.
  # compinit is handled by home-manager with a 24-hour cache; promptinit
  # scans every theme on the fpath which adds ~1.5s to every shell startup.
  programs.zsh = {
    enable = true;
    enableCompletion = false;
    promptInit = "";
  };

  # Determinate Nix manages its own daemon; disable nix-darwin's Nix management
  # to avoid conflicts. This means nix.* settings options are unavailable, but
  # Determinate handles experimental-features and upgrades independently.
  nix.enable = false;

  nixpkgs.hostPlatform = "aarch64-darwin";

  # Required: matches the nix-darwin major version used at first install.
  system.stateVersion = 5;

  system.primaryUser = "andrew";

  # Allow home-manager to replace existing files by backing them up first.
  # After activation, any *.backup files are safe to delete (identical content).
  home-manager.backupFileExtension = "backup";

  users.users.andrew = {
    name = "andrew";
    home = "/Users/andrew";
  };
}
