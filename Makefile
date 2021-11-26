SHELL := /bin/bash

default: help

.PHONY: dependencies
dependencies:
	@command -v stow >/dev/null 2>&1 || brew install stow 2>/dev/null || sudo apt-get install -y stow 2>/dev/null || sudo yum install -y stow 2>/dev/null || { echo >&2 "Please install GNU stow"; exit 1; }

.PHONY: submodules
submodules:
	git submodule update --init

.PHONY: vim-plugins
vim-plugins:
	vim +PlugInstall

.PHONY: stow
stow:
	stow git
	stow misc
	stow ruby
	stow screen
	stow slate
	stow sqlite
	stow tig
	stow tmux
	stow vim
	stow xvim
	stow zsh
	stow lldb
	stow node
	stow xdg_base_directory
	stow asdf
	stow docker
	stow emacs
	stow hammerspoon
	stow bash
	stow sh-common
	stow skhd
	stow homedir.makefile
	stow yabai

.PHONY: link-bin
link-bin:
	@ln -s `pwd`/bin ~/bin

.PHONY: jobs
jobs: ## Install scheduled jobs into the host
	cd jobs && make install

# Set up my language version manager
# https://github.com/asdf-vm/asdf
.PHONY: asdf
asdf:
	@echo "# Setting up asdf"
	[[ -d ~/.asdf ]] || git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.3.0
	[[ -d ~/.asdf/plugins/ruby ]] || ~/.asdf/bin/asdf plugin-add ruby https://github.com/asdf-vm/asdf-ruby.git

.PHONY: install
install: ## Install the entire setup
install: dependencies submodules stow vim-plugins link-bin mac jobs amethyst-install


.PHONY: install-no-vim
install-no-vim: dependencies submodules stow amethyst-install
	@echo 'To setup vim, `make vim-plugins` from a shell'

.PHONY: mac
mac: ## Configure macOS defaults
	sh osx/index.sh

.PHONY: brew-bundle
brew-bundle: ## Bundle Homebrew dependencies
	cd osx && brew bundle

AMETHYST_PREF := ~/Library/Preferences/com.amethyst.Amethyst.plist
.PHONY: amethyst-update
amethyst-update: ## Update the Amethyst config in the machine's ~/Library/Preferences directory
	cp $(AMETHYST_PREF) osx/Preferences/.


.PHONY: amethyst-install
amethyst-install: ## Install the repo's Amethyst preferences into ~/Library/Preferences
	rm -f $(AMETHYST_PREF)
	ln -sf ~/.dotfiles/osx/LaunchAgents/com.andrewsardone.sync-notes.plist $(AMETHYST_PREF)

# via https://gist.github.com/prwhite/8168133
help: ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'
