{ ... }:
{
  system.defaults = {

    # ── NSGlobalDomain ────────────────────────────────────────────────
    NSGlobalDomain = {
      # Expand save and print panels by default
      NSNavPanelExpandedStateForSaveMode = true;
      PMPrintingExpandedStateForPrint = true;
      PMPrintingExpandedStateForPrint2 = true;

      # Full keyboard access for all controls (Tab in modal dialogs)
      AppleKeyboardUIMode = 3;

      # Fastest key repeat; 0 is not a valid nix-darwin value, 1 is minimum
      KeyRepeat = 1;
      InitialKeyRepeat = 10;

      # Subpixel font rendering on non-Apple LCDs
      AppleFontSmoothing = 2;

      # WebKit developer extras (context menu Web Inspector)
      WebKitDeveloperExtras = true;
    };

    # ── Finder ────────────────────────────────────────────────────────
    finder = {
      ShowStatusBar = true;
      ShowPathbar = true;
      ShowExternalHardDrivesOnDesktop = true;
      # Disable quarantine dialog ("Are you sure you want to open…")
      # Mapped to com.apple.LaunchServices LSQuarantine in older scripts.
      # TODO: verify nix-darwin attribute name for LSQuarantine
    };

    # ── Dock ──────────────────────────────────────────────────────────
    dock = {
      expose-animation-duration = 0.1;
      autohide = true;
      autohide-delay = 0.0;
      autohide-time-modifier = 0.5;
      # TODO: expose-group-by-app — verify nix-darwin attr (may be expose-group-apps)
      # expose-group-apps = true;
    };

    # ── Safari ────────────────────────────────────────────────────────
    safari = {
      UniversalSearchEnabled = false;
      SuppressSearchSuggestions = true;
      # TODO: IncludeInternalDebugMenu — verify nix-darwin attr for Safari debug menu
    };

    # ── Mail ─────────────────────────────────────────────────────────
    # TODO: AddressesIncludeNameOnPasteboard — verify nix-darwin mail module attr
    # mail = {
    #   AddressesIncludeNameOnPasteboard = false;
    # };
  };

  # ── Settings without nix-darwin equivalents (apply manually once) ──
  #
  # defaults write NSGlobalDomain NSRepeatCountBinding -string "^u"
  #   Enable Emacs ^u repeat count binding — no nix-darwin attr.
  #
  # defaults write com.apple.LaunchServices LSQuarantine -bool false
  #   Disable "Are you sure?" open dialog — use finder module if attr exists,
  #   otherwise run manually.
  #
  # defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
  #   Prevent .DS_Store on network volumes — no nix-darwin finder attr.
  #
  # defaults write com.jetbrains.intellij.ce ApplePressAndHoldEnabled -bool false
  #   IntelliJ key repeat — no nix-darwin attr. Run manually.
  #
  # defaults write com.google.Chrome DisablePrintPreview -bool true
  # defaults write com.google.Chrome.canary DisablePrintPreview -bool true
  #   Chrome system print dialog — no nix-darwin attr. Run manually.
  #
  # defaults write com.apple.dashboard mcx-disabled -bool true
  #   Disable Dashboard (obsolete on modern macOS, no effect).
  #
  # defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false
  #   Mail address paste format — use mail module if attr exists.
  #
  # defaults write com.apple.dt.Xcode ShowBuildOperationDuration YES
  # defaults write com.apple.dt.Xcode IDEBuildOperationMaxNumberOfConcurrentCompileTasks...
  #   Xcode settings — no nix-darwin attr. Run manually.
  #
  # sudo pmset -a standbydelay 86400
  #   Extend standby delay to 24 hours — no nix-darwin attr. Run with sudo manually.
  #
  # chflags nohidden ~/Library/
  #   Unhide ~/Library — no nix-darwin attr. Run manually once.
  #
  # chsh -s /opt/homebrew/bin/fish
  #   Set fish as default shell. Can also be done via:
  #   users.users.andrew.shell = pkgs.fish; in darwin.nix (requires fish in
  #   /etc/shells, which nix-darwin handles when programs.fish.enable is set
  #   at the system level).
}
