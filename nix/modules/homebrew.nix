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
      "anomalyco/tap"           # opencode
      "autozimu/homebrew-formulas" # unison-fsmonitor
      "clementtsang/bottom"     # bottom
      "felixkratz/formulae"     # borders, sketchybar
      "koekeishiya/formulae"    # yabai, skhd (legacy, kept for reference)
      "nikitabobko/tap"         # aerospace cask
      "saulpw/vd"               # visidata
      "yqrashawn/goku"          # goku (karabiner DSL)
    ];

    # CLI formulae not available in nixpkgs, or macOS-specific.
    # Tools that exist in nixpkgs are in nix/modules/packages.nix instead.
    brews = [
      # Window border renderer (FelixKratz custom tap)
      "felixkratz/formulae/borders"
      # Status bar (FelixKratz custom tap)
      "felixkratz/formulae/sketchybar"

      # macOS-specific / not in nixpkgs
      "choose-gui"
      "colima"
      "dasht"
      "dateutils"
      "docker"
      "exif"
      "grc"
      "ical-buddy"
      "m-cli"
      "macos-trash"
      "markedit"
      "mole"
      "node@22"
      "openjdk@11"
      "pass"
      "pipes-sh"
      "reattach-to-user-namespace"
      "shpotify"
      "spark"
      "spotify_player"
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
      "saulpw/vd/visidata"
      "yqrashawn/goku/goku"
      "anomalyco/tap/opencode"

      # Legacy window manager (kept, inactive)
      "koekeishiya/formulae/yabai"
      "koekeishiya/formulae/skhd"

      # Other tools not confidently in nixpkgs
      "align"
      "doctl"
      "the_silver_searcher"
      "parallel"
      "prettier"
      "speedtest-cli"
      "watchman"
    ];

    casks = [
      "1password"
      "amazon-workspaces"
      "appcleaner"
      "arq"
      "boop"
      "calibre"
      "claude"
      "cleanshot"
      "discord"
      "figma"
      "firefox"
      "flux"
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
      "font-sf-mono-nerd-font"
      "gfxcardstatus"
      "google-chrome"
      "hammerspoon"
      "homerow"
      "hook"
      "imageoptim"
      "karabiner-elements"
      "keyboard-cleaner"
      "keyboard-maestro"
      "kindle"
      "kitty"
      "launchbar"
      "launchcontrol"
      "leader-key"
      "licecap"
      "little-snitch"
      "macdown"
      "maestral"
      "menubar-colors"
      "micro-snitch"
      "microsoft-auto-update"
      "microsoft-office"
      "middleclick"
      "mimestream"
      "monitorcontrol"
      "multi"
      "nikitabobko/tap/aerospace"
      "obsidian"
      "pocket-casts"
      "quicklook-json"
      "quip"
      "rectangle"
      "sensiblesidebuttons"
      "sf-symbols"
      "shortcat"
      "signal"
      "sketch"
      "slack"
      "soulver"
      "spotify"
      "stats"
      "superduper"
      "tailscale"
      "tempbox"
      "thaw"
      "topnotch"
      "transmission"
      "vimr"
      "viu"
      "vlc"
      "whichspace"
      "xbar"
      "zoom"
    ];

    masApps = {
      "Adobe Lightroom"  = 1451544217;
      "Amphetamine"      = 937984704;
      "Day One"          = 1055511498;
      "Drafts"           = 1435957248;
      "Dropover"         = 1355679052;
      "Fantastical"      = 975937182;
      "GarageBand"       = 682658836;
      "Gestimer"         = 990588172;
      "HEIC Converter"   = 1294126402;
      "iMovie"           = 408981434;
      "Instapaper"       = 288545208;
      "Irvue"            = 1039633667;
      "Jiffy"            = 1502527999;
      "Keynote"          = 409183694;
      "Mic Drop"         = 1489816366;
      "MindNode"         = 992076693;
      "Numbers"          = 409203825;
      "Nutshell"         = 955864065;
      "Pages"            = 409201541;
      "Parcel"           = 639968404;
      "Pixelmator Pro"   = 1289583905;
      "Poolsuite FM"     = 1514817810;
      "Reeder"           = 1529448980;
      "Things"           = 904280696;
      "Vimari"           = 1480933944;
    };
  };
}
