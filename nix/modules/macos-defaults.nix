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

      AppleInterfaceStyleSwitchesAutomatically = true;

    };

    # ── Finder ────────────────────────────────────────────────────────
    finder = {
      ShowStatusBar = true;
      ShowPathbar = true;
      FXPreferredViewStyle = "Nlsv";  # list view
      NewWindowTarget = "Home";
      ShowExternalHardDrivesOnDesktop = false;
      ShowRemovableMediaOnDesktop = false;
      ShowMountedServersOnDesktop = false;
      ShowHardDrivesOnDesktop = false;
      # Disable quarantine dialog ("Are you sure you want to open…")
      # Mapped to com.apple.LaunchServices LSQuarantine in older scripts.
      # TODO: verify nix-darwin attribute name for LSQuarantine
    };

    # ── Dock ──────────────────────────────────────────────────────────
    dock = {
      expose-animation-duration = 0.1;
      autohide = false;
      autohide-delay = 0.0;
      autohide-time-modifier = 0.5;
      orientation = "bottom";
      tilesize = 20;
      magnification = false;
      # TODO: expose-group-by-app — verify nix-darwin attr (may be expose-group-apps)
      # expose-group-apps = true;
      persistent-apps = [
        "/System/Library/CoreServices/Finder.app"
        "/Applications/Obsidian.app"
        "/System/Applications/Mail.app"
        "/System/Applications/Calendar.app"
        "/System/Applications/Reminders.app"
        "/Applications/kitty.app"
        "/Applications/Firefox.app"
        "/System/Applications/Messages.app"
        "/Applications/Signal.app"
        "/Applications/WhatsApp.app"
        "/Applications/Discord.app"
        "/Applications/Claude.app"
        "/Applications/Day One.app"
      ];
    };

    # ── Trackpad ──────────────────────────────────────────────────────
    trackpad = {
      Clicking = true;  # tap to click
      TrackpadThreeFingerDrag = true;
    };

    # ── Mail ─────────────────────────────────────────────────────────
    # TODO: AddressesIncludeNameOnPasteboard — verify nix-darwin mail module attr
    # mail = {
    #   AddressesIncludeNameOnPasteboard = false;
    # };

    # ── Menu Bar Clock ──────────────────────────────────────────────
    menuExtraClock = {
      Show24Hour = true;
      ShowDate = 2; # 0 = when space allows, 1 = always, 2 = never
      ShowSeconds = false;
    };

    # ── Accessibility ─────────────────────────────────────────────────
    # Hold Control and scroll to zoom the screen.
    universalaccess.closeViewScrollWheelToggle = true;

    # ── Control Center ────────────────────────────────────────────────
    controlcenter = {
      AirDrop = false;
      Bluetooth = false;
      Display = false;
      FocusModes = false;
      NowPlaying = false;
      Sound = true;
    };
  };

  # ── Timezone ──────────────────────────────────────────────────────
  time.timeZone = "America/New_York";

  system.defaults.CustomUserPreferences = {
    # Control (⌃) as the scroll-to-zoom modifier (1 << 18 = 262144).
    "com.apple.universalaccess" = {
      HIDScrollZoomModifierMask = 262144;
      closeViewSmoothImages = false;  # uncheck "Smooth images" → crisp, pixelated zoom
    };
    # Disable macOS screenshot keyboard shortcuts in favor of CleanShot X.
    "com.apple.symbolichotkeys" = {
      AppleSymbolicHotKeys = {
        "28"  = { enabled = false; };  # ⌘⇧3  screenshot to file
        "29"  = { enabled = false; };  # ⌘⌃⇧3 screenshot to clipboard
        "30"  = { enabled = false; };  # ⌘⇧4  area to file
        "31"  = { enabled = false; };  # ⌘⌃⇧4 area to clipboard
        "184" = { enabled = false; };  # ⌘⇧5  screenshot & recording options
        # Show Desktop (Exposé) → ⌃⌘3   ascii=51 keycode=20 mods=ctrl+cmd(1310720)
        "36" = {
          enabled = true;
          value = {
            parameters = [ 51 20 1310720 ];
            type = "standard";
          };
        };
      };
    };
  };

  # ── Settings without nix-darwin equivalents (apply manually once) ──
  #
  # defaults write com.apple.Safari UniversalSearchEnabled -bool false
  # defaults write com.apple.Safari SuppressSearchSuggestions -bool true
  #   Safari search settings — system.defaults.safari removed from nix-darwin.
  #   Apply manually.
  #
  # defaults delete NSGlobalDomain NSRepeatCountBinding
  #   Disable Emacs ^u repeat count binding — no nix-darwin attr.
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
