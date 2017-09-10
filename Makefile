default: install

dependencies:
	@command -v stow >/dev/null 2>&1 || brew install stow 2>/dev/null || sudo apt-get install -y stow 2>/dev/null || sudo yum install -y stow 2>/dev/null || { echo >&2 "Please install GNU stow"; exit 1; }

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
	stow xdg_base_directory
	stow asdf

link-bin:
	@ln -s `pwd`/bin ~/bin

# Set up my language version manager
# https://github.com/asdf-vm/asdf
asdf:
	@echo "## Setting up asdf"
	[[ -d ~/.asdf ]] || git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.3.0
	[[ -d ~/.asdf/plugins/ruby ]] || ~/.asdf/bin/asdf plugin-add ruby https://github.com/asdf-vm/asdf-ruby.git

keyboard:
	@ln -s `pwd`/vendor/flav-hammerspoon ~/.hammerspoon

install: dependencies submodules stow vim-plugins keyboard link-bin mac

install-no-vim: dependencies submodules stow
	@echo 'To setup vim, `make vim-plugins` from a shell'

mac:
	sh osx/index.sh
