{ ... }:
{
  homebrew = {
    enable = true;

    onActivation = {
      # Remove any Homebrew package not declared here on `darwin-rebuild switch`.
      cleanup = "zap";
      autoUpdate = true;
      upgrade = true;
    };

    taps = [
      "anomalyco/tap"              # opencode
      "autozimu/homebrew-formulas" # unison-fsmonitor
      "clementtsang/bottom"        # bottom
"yqrashawn/goku"             # goku (karabiner DSL)
    ];

    # CLI formulae not available in nixpkgs, or macOS-specific.
    # Tools that exist in nixpkgs are in nix/modules/packages.nix instead.
    brews = [
      # macOS-specific / not in nixpkgs
      "colima"
      "dateutils"
      "docker"
      "exif"
      "m-cli"
      "macos-trash"
      "mole"
      "node@22"
      "openjdk@11"
      "pipes-sh"
      "reattach-to-user-namespace"
      "spark"
      "switchaudio-osx"
      "terminal-notifier"
      "toipe"
      "unison"
      "autozimu/homebrew-formulas/unison-fsmonitor"
      "xcodes"

      # Language runtimes (specific versions)
      "python@3.9"
      "python@3.10"
      "ruby"
      "yarn"
      "pnpm"

      # Custom taps
      "yqrashawn/goku/goku"
      "anomalyco/tap/opencode"

      # Other tools not confidently in nixpkgs
      "align"
      "doctl"
      "the_silver_searcher"
      "parallel"
      "prettier"
    ];

    casks = [
      "1password"
      "appcleaner"
      "arq"
      "boop"
      "calibre"
      "claude"
      "cleanshot"
      "discord"
      "figma"
      "firefox"
      "font-awesome-terminal-fonts"
      "font-fira-code"
      "font-fontawesome"
      "font-hack"
      "font-hack-nerd-font"
      "font-ia-writer-duo"
      "font-ia-writer-duospace"
      "font-ia-writer-mono"
      "font-ia-writer-quattro"
      "font-ibm-plex"
      "font-inter"
      "font-iosevka"
      "font-iosevka-nerd-font"
      "font-jetbrains-mono"
      "font-jetbrains-mono-nerd-font"
      "font-kreon"
      "font-lato"
      "font-league-gothic"
      "font-oswald"
      "font-poppins"
      "font-pt-mono"
      "font-pt-sans"
      "font-pt-serif"
      "google-chrome"
      "google-drive"
      "homerow"
      "hook"
      "imageoptim"
      "karabiner-elements"
      "keyboard-cleaner"
      "keyboard-maestro"
      "kitty"
      "launchbar"
      "launchcontrol"
      "macdown"
      "maestral"
      "menubar-colors"
      "middleclick"
      "monitorcontrol"
      "netnewswire"
      "obsidian"
      "pocket-casts"
      "rectangle"
      "sensiblesidebuttons"
      "sf-symbols"
      "signal"
      "slack"
      "soulver"
      "spotify"
      "tailscale-app"
      "thaw"
      "vimr"
      "vlc"
      "zoom"
    ];

    masApps = {
      "Adobe Lightroom"  = 1451544217;
      "Amphetamine"      = 937984704;
      "Day One"          = 1055511498;
      "Drafts"           = 1435957248;
      "Dropover"         = 1355679052;
      "HEIC Converter"   = 1294126402;
      "Pixelmator Pro"   = 1289583905;
      "Poolsuite FM"     = 1514817810;
    };
  };
}
