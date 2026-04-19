# AGENTS.md

Instructions for AI coding agents (Claude Code and similar) working in this repo.

---

## Repo layout

This is a regular git repository. It contains two kinds of things side by side:

1. **Dotfiles** — the actual config files (`.zshrc`, `.gitconfig`, `.tmux.conf`,
   `.config/fish/config.fish`, `.config/sketchybar/`, `.hammerspoon/`, etc.)
2. **Nix configuration** — `flake.nix` and `nix/` which use nix-darwin and
   home-manager to deploy those dotfiles and manage the macOS system

home-manager deploys dotfiles by creating symlinks in `$HOME` that point back
into the repo checkout (via `config.lib.file.mkOutOfStoreSymlink`). The repo
must be cloned to `~/dotfiles` — this path is defined in
`nix/modules/dotfile-links.nix` as `repoPath`.

---

## How to apply changes

```sh
darwin-rebuild switch --flake ~/dotfiles#personal-mbp
```

Only the owner of the machine can run this. Agents should never run it.

---

## How to validate without applying

```sh
# Check flake evaluation (syntax + type errors)
nix flake check ~/dotfiles

# Build without switching (downloads closures, verifies linkage)
nix build ~/dotfiles#darwinConfigurations.personal-mbp.system
```

These are safe to run. Run them after any change to `nix/` files.

---

## Commit conventions

Follow [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).

**Format:** `<type>(<scope>): <subject>`

**Subject:** ≤50 chars, imperative mood, no period, lowercase after prefix.

**Body:** Wrap at 72 chars. Explain _what_ and _why_, not _how_.

**Types:** `feat`, `fix`, `refactor`, `docs`, `chore`, `test`, `ci`

**Scopes for this repo:** `flake`, `homebrew`, `macos-defaults`, `packages`,
`git`, `shell`, `tmux`, `dotfile-links`, `readme`, `agents`

**Multiple commits:** Split by concern. 3+ files changed → 2+ commits.
5+ files → 3+ commits. Never squash or amend after committing.

---

## What not to touch

Do not delete, overwrite, or reorganize these files — they are the active
dotfiles or are retained for historical reference:

- **Active dotfiles** managed as symlinks: edit in place in the repo,
  changes are live immediately (no rebuild needed)
- **`nix/modules/dotfile-links.nix`** `repoPath` variable — do not change
  without also updating `README.md` bootstrap instructions to match

---

## Adding a new package

1. **GUI app or font?** → Add to `homebrew.casks` in `nix/modules/homebrew.nix`
2. **CLI tool available in nixpkgs?** → Add to `environment.systemPackages`
   in `nix/modules/packages.nix`
3. **CLI tool NOT in nixpkgs, or from a custom tap?** → Add to `homebrew.brews`
   in `nix/modules/homebrew.nix` (add the tap to `homebrew.taps` if needed)
4. **App Store app?** → Add to `homebrew.masApps` in `nix/modules/homebrew.nix`
   with `"App Name" = <numeric-id>` syntax

To check if a package exists in nixpkgs:

```sh
nix search nixpkgs <name>
```

After any change to `nix/` files, validate before committing:

```sh
nix flake check ~/dotfiles
```

---

## Updating flake inputs

```sh
cd ~/dotfiles
nix flake update                  # update all inputs (nixpkgs, nix-darwin, home-manager)
nix flake update nixpkgs          # update a single input
# validate, then commit the lock file
nix flake check
git add flake.lock
git commit -m "chore(flake): update inputs"
```

---

## Adding a new dotfile link

All `home.file` entries live in `nix/modules/dotfile-links.nix`. Use the
`link` helper for simple paths:

```nix
home.file.".some-file".source =
  config.lib.file.mkOutOfStoreSymlink "${repoPath}/.some-file";
```

Or use the shorthand for paths that don't need an inline comment:

```nix
home.file.".some-config" = link ".some-config";
```

**Never** use plain `.source = ./path` for dotfiles — that copies files into
the Nix store and breaks in-place editing.

---

## Never push

Agents must never run `git push` or any command that modifies a remote
(`git push`, `git push --force`, `gh pr create` that triggers a push, etc.).

All commits are local only. Pushing is the owner's responsibility after
reviewing the final state.
