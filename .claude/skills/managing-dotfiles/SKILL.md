---
name: managing-dotfiles
description: >
  Manages dotfiles tracked in a bare git repository at ~/.dotfiles.git.
  Use this skill whenever the user mentions dotfiles, wants to track a config
  file, or asks for git operations (status, diff, add, commit, push, pull,
  branch) on files like .zshrc, .gitconfig, .vimrc, .tmux.conf, or any
  other file that lives in $HOME and is version-controlled. Trigger on phrases
  like "add to my dotfiles", "commit my zshrc changes", "track this config
  file", "dotfiles status", "push my dotfiles", or "check what dotfiles have
  changed".
---

# Managing Dotfiles

This user tracks dotfiles using a **bare git repository** — a setup where the
git directory and the working tree are separate. The user has a `config` shell
alias, but agents must use the full git command form since aliases are not
available in non-interactive shells.

## The Setup

- **Git dir**: `~/.dotfiles.git` (bare repo)
- **Work-tree**: `$HOME`
- **Remote**: `https://github.com/andrewsardone/dotfiles.git`
- **Default branch**: `master`
- **Git command prefix**: `git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME`
  (the user's `config` alias is equivalent to this)

Because `showUntrackedFiles = no` is set, status only shows files that are
already tracked. New files are invisible until explicitly added.

## Common Operations

Replace `DF` below with `git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME`:

| Task | Command |
|------|---------|
| Check what's changed | `DF status` |
| Review a diff | `DF diff` |
| Start tracking a new file | `DF add ~/.filename` |
| Stage changes to a tracked file | `DF add ~/.filename` |
| Commit | `DF commit -m "..."` |
| Push (only when asked) | `DF push` |
| Pull (only when asked) | `DF pull` |
| List all tracked files | `DF ls-files` |
| Show recent history | `DF log --oneline` |

## Rules

**Always spell out `git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME`.**
Running plain `git` in `$HOME` will either do nothing or operate on the wrong
repo. Shell aliases and variables don't expand properly in non-interactive
shells, so inline the full command every time.

**Never push automatically.** Push and pull are always explicit — commit and
stop. Only run push or pull if the user specifically asks.

**Use full paths.** Reference files as `~/.zshrc` rather than relative paths,
since the work-tree is `$HOME` and commands may be run from any directory.

**New files don't appear in status.** If the user asks why a file isn't
showing up, it's because `showUntrackedFiles = no` hides anything not yet
tracked. The fix is `DF add <path>` to start tracking it.

## Branching

Most dotfile changes go straight to `master` — tweaks and small iterations
don't need branches. For a larger batch of related changes, a feature branch
makes sense. Follow the user's lead; don't push for PRs on one-liner changes.
