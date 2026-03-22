---
name: managing-dotfiles
description: >
  Manages dotfiles tracked in a bare git repository using the `config` alias.
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
git directory and the working tree are separate. All git operations go through
the `config` alias, not `git`.

## The Setup

- **Git dir**: `~/.dotfiles.git` (bare repo)
- **Work-tree**: `$HOME`
- **Remote**: `https://github.com/andrewsardone/dotfiles.git`
- **Default branch**: `master`
- **`config` alias**: equivalent to
  `git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME`

Because `showUntrackedFiles = no` is set, `config status` only shows files
that are already tracked. New files are invisible until explicitly added.

## Common Operations

| Task | Command |
|------|---------|
| Check what's changed | `config status` |
| Review a diff | `config diff` |
| Start tracking a new file | `config add ~/.filename` |
| Stage changes to a tracked file | `config add ~/.filename` |
| Commit | `config commit -m "..."` |
| Push (only when asked) | `config push` |
| Pull (only when asked) | `config pull` |
| List all tracked files | `config ls-files` |
| Show recent history | `config log --oneline` |

## Rules

**Always use `config`, never `git`.** The alias carries the `--git-dir` and
`--work-tree` flags that make the bare repo work. Running `git` in `$HOME`
will either do nothing or operate on the wrong repo.

**Never push automatically.** Push and pull are always explicit — commit and
stop. Only run `config push` or `config pull` if the user specifically asks.

**Use full paths.** Reference files as `~/.zshrc` rather than relative paths,
since the work-tree is `$HOME` and commands may be run from any directory.

**New files don't appear in status.** If the user asks why a file isn't
showing up, it's because `showUntrackedFiles = no` hides anything not yet
tracked. The fix is `config add <path>` to start tracking it.

## Branching

Most dotfile changes go straight to `master` — tweaks and small iterations
don't need branches. For a larger batch of related changes, a feature branch
makes sense. Follow the user's lead; don't push for PRs on one-liner changes.
