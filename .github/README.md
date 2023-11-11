These are my dotfiles. There are many like them, but these ones are mine.

## Installation

    git clone --bare https://github.com/andrewsardone/dotfiles.git $HOME/.dotfiles.git
    alias config='git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME'
    config checkout

## Bullet Points

- `vim` & `zsh`/`bash` as much as possible
- Using the [bare git repo method](https://www.atlassian.com/git/tutorials/dotfiles)
- [homebrew](https://brew.sh/), [homebrew-cask](https://caskroom.github.io/), and
  [homebrew-bundle](https://github.com/Homebrew/homebrew-bundle) for managing my macOS
  dependencies
