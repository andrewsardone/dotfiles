default: install

dependencies: install-homebrew
	@command -v stow >/dev/null 2>&1 || brew install stow 2>/dev/null || sudo apt-get install -y stow 2>/dev/null || sudo yum install -y stow 2>/dev/null || { echo >&2 "Please install GNU stow"; exit 1; }

install-homebrew:
	sh osx/install-homebrew.sh

submodules:
	git submodule update --init

vim-plugins: submodules
	vim +PluginInstall +qall

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

install: dependencies submodules stow vim-plugins

install-no-vim: dependencies submodules stow
	@echo 'To setup vim, `make vim-plugins` from a shell'

mac: install
	sh osx/index.sh
