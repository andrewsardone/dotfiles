# dotfiles

A regular git repository containing both the dotfiles themselves and the Nix
configuration that deploys them. nix-darwin manages the macOS system layer
(Homebrew, system preferences, nixpkgs CLI tools) and home-manager deploys
dotfiles into `$HOME` as symlinks that point back into the repo checkout —
so edits to any dotfile take effect immediately without a rebuild.

---

## Architecture

```
flake.nix
├── nix-darwin  (system layer — runs as root on darwin-rebuild switch)
│   ├── nix/modules/macos-defaults.nix   macOS system.defaults preferences
│   ├── nix/modules/homebrew.nix         GUI apps + non-nixpkgs CLI tools
│   └── nix/modules/packages.nix         CLI tools from nixpkgs-unstable
└── home-manager  (user layer — runs as andrew)
    ├── nix/modules/git.nix              programs.git (migrated from .gitconfig)
    ├── nix/modules/shell.nix            programs.zsh + programs.starship
    ├── nix/modules/tmux.nix             programs.tmux (migrated from .tmux.conf)
    └── nix/modules/dotfile-links.nix    home.file symlinks → repo checkout
```

All `home.file` entries use `config.lib.file.mkOutOfStoreSymlink` so the
symlinks point to `~/.dotfiles/<file>` rather than into the Nix store. This
means you can edit `.aerospace.toml`, Hammerspoon scripts, sketchybar config,
etc. in place and changes take effect without running `darwin-rebuild switch`.

---

## What's managed where

| Thing                        | Managed by                                                         |
| ---------------------------- | ------------------------------------------------------------------ |
| macOS system preferences     | nix-darwin `system.defaults` (macos-defaults.nix)                  |
| GUI apps, fonts              | Homebrew casks (homebrew.nix)                                      |
| CLI tools with no Nix module | Homebrew brews (homebrew.nix)                                      |
| App Store apps               | Homebrew `masApps` (homebrew.nix)                                  |
| CLI tools in nixpkgs         | nixpkgs (packages.nix)                                             |
| git config                   | home-manager `programs.git` (git.nix)                              |
| zsh config                   | home-manager `programs.zsh` (shell.nix)                            |
| starship prompt              | home-manager `programs.starship` + `.config/starship.toml` symlink |
| tmux config                  | home-manager `programs.tmux` (tmux.nix)                            |
| All other dotfiles           | home-manager `home.file` symlinks (dotfile-links.nix)              |

---

## Bootstrap (new machine)

### 1. Install Nix

Use the Determinate Systems installer (handles Apple Silicon, sets up
`/nix`, enables flakes automatically):

```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

Close and reopen your terminal after installation.

### 2. Clone this repo

The repo **must** be cloned to `~/.dotfiles`. This path is hardcoded in
`nix/modules/dotfile-links.nix`. If you want a different location, update the
`repoPath` variable in that file and update these instructions to match.

```sh
git clone https://github.com/andrewsardone/dotfiles ~/.dotfiles
cd ~/.dotfiles
```

### 3. Bootstrap nix-darwin

The first run uses `nix run` because `darwin-rebuild` doesn't exist yet:

```sh
nix run nix-darwin -- switch --flake .#personal-mbp
```

This will:

- Build and link nix-darwin
- Run Homebrew to install casks, brews, and App Store apps (`mas` must be
  signed in to the App Store first)
- Deploy dotfile symlinks via home-manager
- Apply macOS system preferences

### 4. Apply manual macOS settings (one-time)

These have no nix-darwin equivalent:

```sh
# Emacs-style repeat count binding in macOS text fields
defaults write NSGlobalDomain NSRepeatCountBinding -string "^u"

# Disable quarantine ("Are you sure you want to open?") dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false

# Disable press-and-hold in IntelliJ (enable key repeat)
defaults write com.jetbrains.intellij.ce ApplePressAndHoldEnabled -bool false

# Chrome: use system print dialog
defaults write com.google.Chrome DisablePrintPreview -bool true

# Unhide ~/Library in Finder
chflags nohidden ~/Library/

# Extend standby delay to 24 hours (prevents sleep-discharge on lid close)
sudo pmset -a standbydelay 86400
```

---

## Day-to-day usage

### Apply Nix changes

```sh
darwin-rebuild switch --flake ~/.dotfiles#personal-mbp
```

### Edit a dotfile (no rebuild needed)

All dotfiles managed via `home.file` are symlinks into the repo. Just edit
the file in `~/.dotfiles/` (or via the symlink — same thing) and the change
is live immediately.

```sh
# Example: edit sketchybar config
vim ~/.dotfiles/.config/sketchybar/sketchybarrc
# Changes are live; restart sketchybar if needed:
sketchybar --reload
```

### Add a new package

| Package type            | Where to add                                              |
| ----------------------- | --------------------------------------------------------- |
| GUI app or font         | `nix/modules/homebrew.nix` → `homebrew.casks`             |
| CLI tool in nixpkgs     | `nix/modules/packages.nix` → `environment.systemPackages` |
| CLI tool NOT in nixpkgs | `nix/modules/homebrew.nix` → `homebrew.brews`             |
| App Store app           | `nix/modules/homebrew.nix` → `homebrew.masApps`           |

Then run `darwin-rebuild switch --flake ~/.dotfiles#personal-mbp`.

### Update flake inputs

```sh
cd ~/.dotfiles
nix flake update           # updates flake.lock (all inputs)
# Or update a single input:
nix flake update nixpkgs
darwin-rebuild switch --flake ~/.dotfiles#personal-mbp
git add flake.lock && git commit -m "chore(flake): update inputs"
```

### Validate without applying

```sh
nix flake check ~/.dotfiles
nix build ~/.dotfiles#darwinConfigurations.personal-mbp.system
```

---

## Known issues / TODOs

- `silver-searcher` (the `ag` command, Brewfile: `the_silver_searcher`) is
  commented out in packages.nix. Verify the correct nixpkgs attribute name
  and uncomment if available.
- `neofetch` is unmaintained upstream. If removed from nixpkgs, replace with
  `fastfetch` in packages.nix.
- Several macOS defaults in macos-defaults.nix have `TODO:` comments where
  the nix-darwin attribute name is uncertain. Verify against the installed
  nix-darwin version with:

  ```sh
  man 5 darwin-configuration
  ```

- `nix flake check` has not been run in the build environment where this
  config was authored. Run it after cloning and resolve any evaluation errors
  before the first `darwin-rebuild switch`.
