default: install

dependencies:
	@command -v stow >/dev/null 2>&1 || { echo >&2 "Please install GNU stow"; exit 1; }

submodules:
	git submodule update --init

install: dependencies submodules
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
