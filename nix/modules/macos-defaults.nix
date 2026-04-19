{ ... }:
{
  system.defaults = {

    # ── NSGlobalDomain ────────────────────────────────────────────────
    NSGlobalDomain = {
      # Expand save and print panels by default
      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
      PMPrintingExpandedStateForPrint = true;
      PMPrintingExpandedStateForPrint2 = true;

      # Full keyboard access for all controls (Tab in modal dialogs)
      AppleKeyboardUIMode = 3;

      # Fastest key repeat rate; initial repeat delay uses default
      KeyRepeat = 1; # 0 is not a valid nix-darwin value; 1 is fastest supported
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
      # Allow text selection in Quick Look
      QLEnableTextSelection = true;
      # Avoid .DS_Store on network volumes
      # NOTE: maps to com.apple.desktopservices DSDontWriteNetworkStores
    };

    # ── Dock ──────────────────────────────────────────────────────────
    dock = {
      # Speed up Mission Control animations
      expose-animation-duration = 0.1;
      # Group windows by application in Mission Control
      expose-group-by-app = true;
      # Auto-hide with no delay
      autohide = true;
      autohide-delay = 0.0;
      autohide-time-modifier = 0.5;
    };

    # ── Safari ────────────────────────────────────────────────────────
    safari = {
      UniversalSearchEnabled = false;
      SuppressSearchSuggestions = true;
      # Developer menu / debug
      IncludeInternalDebugMenu = true;
    };

    # ── Mail ─────────────────────────────────────────────────────────
    mail = {
      AddressesIncludeNameOnPasteboard = false;
    };

    # ── Login window / miscellaneous ─────────────────────────────────
    loginwindow = { };

    screensaver = { };
  };

  # ── Settings without nix-darwin equivalents (apply manually) ──────
  #
  # defaults write NSGlobalDomain NSRepeatCountBinding -string "^u"
  #   Enable key repeat count binding (Emacs ^u universal-argument)
  #   No nix-darwin attribute; run manually once.
  #
  # defaults write com.apple.LaunchServices LSQuarantine -bool false
  #   Disable "Are you sure you want to open this application?" dialog
  #   No nix-darwin attribute; run manually once.
  #
  # defaults write com.jetbrains.intellij.ce ApplePressAndHoldEnabled -bool false
  #   Disable press-and-hold for IntelliJ IDEA key repeat
  #   No nix-darwin attribute; run manually once.
  #
  # defaults write com.google.Chrome DisablePrintPreview -bool true
  # defaults write com.google.Chrome.canary DisablePrintPreview -bool true
  #   Use system print dialog in Chrome
  #   No nix-darwin attribute; run manually once.
  #
  # defaults write com.apple.dashboard mcx-disabled -bool true
  #   Disable Dashboard (obsolete on modern macOS)
  #   No nix-darwin attribute.
  #
  # defaults write com.apple.dt.Xcode ShowBuildOperationDuration YES
  # defaults write com.apple.dt.Xcode IDEBuildOperationMaxNumberOfConcurrentCompileTasks ...
  #   Xcode build time display and parallelism
  #   No nix-darwin attribute; run manually if needed.
  #
  # sudo pmset -a standbydelay 86400
  #   Extend standby delay to 24 hours
  #   No nix-darwin attribute; run manually with sudo.
  #
  # chflags nohidden ~/Library/
  #   Unhide ~/Library in Finder
  #   No nix-darwin attribute; run manually once.
  #
  # chsh -s /opt/homebrew/bin/fish
  #   Set fish as default shell
  #   Handle via users.users.andrew.shell in darwin.nix if desired,
  #   or run manually.
}
