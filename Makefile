default: install

dependencies:
	@command -v stow >/dev/null 2>&1 || brew install stow 2>/dev/null || { echo >&2 "Please install GNU stow"; exit 1; }

submodules:
	git submodule update --init

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

install: dependencies submodules stow
