# Source the shell scripts and such that I share between bash and zsh.
export TERM="xterm-256color"

sh_common_dir=$HOME/.sh-include
if [ -d $sh_common_dir ]; then
  for include in $sh_common_dir/*; do
    source "$include"
  done
fi

fpath=( "$HOME/.zfunctions" $fpath )

# zshrc reloading
alias reload!='source ~/.zshrc'

# 10ms for key sequences
KEYTIMEOUT=1

# colors
export LSCOLORS="exfxcxdxbxegedabagacad"
export CLICOLOR=true

# lolXcode
alias ded='rm -rf ~/Library/Developer/Xcode/DerivedData/'

# editor / readline setup
export EDITOR=nvim
bindkey -v
bindkey '^R' history-incremental-search-backward # a must have
if type /usr/local/bin/vim > /dev/null 2>&1; then
  alias vi='/usr/local/bin/vim'
fi
if type nvim > /dev/null 2>&1; then
  alias vim='nvim'
  alias vi='nvim'
fi

function block_cursor() {
  if [ ! -z "$TMUX" ]; then
    print -n -- "\EPtmux;\E\E]50;CursorShape=0\C-G\E\\"
  fi
}
function line_cursor() {
  if [ ! -z "$TMUX" ]; then
    print -n -- "\EPtmux;\E\E]50;CursorShape=1\C-G\E\\"
  fi
}

function zle-keymap-select zle-line-init {
  # change cursor shape in iTerm2 and in tmux
  case $KEYMAP in
    vicmd)      block_cursor;;
    viins|main) line_cursor;;
  esac

  zle reset-prompt
  zle -R
}

function zle-line-finish {
  block_cursor
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

# C-x C-e bash-ism to edit the current command
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line
bindkey '\C-x\C-e' edit-command-line

# prompt
eval "$(starship init zsh)"

# pure prompt config
PURE_GIT_PULL=0

# Fish shell like syntax highlighting for Zsh
source $HOME/.zfunctions/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# history
setopt APPEND_HISTORY # adds history
# adds history incrementally and share it across sessions
setopt INC_APPEND_HISTORY SHARE_HISTORY
# don't record dupes in history
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
export HISTSIZE=100000 SAVEHIST=100000 HISTFILE=~/.zhistory

# pager
export LESS="-RIM"

# nutshell
NUB_BIN="$HOME/source/nutshell/nub/bin/nub"
[ -f $NUB_BIN ] && eval "$($NUB_BIN init -)"

# autocomplete
autoload -U compinit && compinit

# Task management

# list TODO/FIX lines from the current project
alias todos="ack --nogroup '(TODO|FIX(ME)?):'"

# create a Taskpaper todo file in the current folder
# via http://brettterpstra.com/2013/03/31/a-few-more-of-my-favorite-shell-aliases/
alias tp='touch todo.taskpaper && vi todo.taskpaper'

# grc

command -v brew >/dev/null 2>&1 && GRC_BASHRC="`brew --prefix`/etc/grc.bashrc"
if [ -f "$GRC_BASHRC" ]; then
  source $GRC_BASHRC
fi

# via http://thomashunter.name/blog/removing-duplicate-entries-from-finders-open-with-menu/
alias reset-finder-open-with-menu="/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister -kill -r -domain local -domain system -domain user; killall Finder"

# hub, which makes git better with github http://defunkt.io/hub/
command -v hub >/dev/null 2>&1 && alias git=hub

# Filesystem marks
# via http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
# with some pushd modifications
export MARKPATH=$HOME/.marks
function jump {
    f "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
function mark {
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark {
    rm -i "$MARKPATH/$1"
}
function marks {
    \ls -l "$MARKPATH" | tail -n +2 | sed 's/  / /g' | cut -d' ' -f9- | awk -F ' -> ' '{printf "%-10s -> %s\n", $1, $2}'
}
function _completemarks {
  reply=($(ls $MARKPATH))
}

compctl -K _completemarks jump
compctl -K _completemarks unmark


# json processing via `jsc`
# Example:
#   > json=$(curl -s 'http://httpbin.org/ip') && jsc -e "var json = $json; print(json['origin']);"
#   98.250.104.203
alias jsc=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc

# Go path setup
export GOPATH=$HOME/source/gopath
export PATH=$PATH:$GOPATH/bin

nutshell_zshrc="$HOME/.nutshell.zshrc"
[ -f $nutshell_zshrc ] && source $nutshell_zshrc

# asdf – Extendable programming language version manager
# https://github.com/asdf-vm/asdf
asdf_sh=$HOME/.asdf/asdf.sh
[ -f $asdf_sh ] && source $asdf_sh
asdf_completions_sh=$HOME/.asdf/completions/asdf.bash
[ -f $asdf_completions_sh ] && source $asdf_completions_sh

# fzf
# https://github.com/junegunn/fzf
export FZF_DEFAULT_OPTS='--height 40% --reverse --border --multi'
export FZF_DEFAULT_COMMAND='ag -g ""'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export GPG_TTY=$(tty)

# pass
# https://www.passwordstore.org/

# alias `pass` to fix the following error:
#   sed: RE error: illegal byte sequence
# See https://stackoverflow.com/a/23584470 for more info
alias pass='LC_ALL=C pass'

docker_func=$HOME/.dockerfunc
[ -f $docker_func ] && source $docker_func

# Misc specific env files to include
# h/t Flav https://github.com/flav/dotfiles/blob/633dc88b79652ae38319db22f53dadd9054f21b8/stow/bash/.profile
if [ -d ~/.vault/env ]; then
  for e in ~/.vault/env/*.env; do
    source $e;
  done
fi

# gcloud setup
gcloud_include='/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
[ -f $gcloud_include ] && source $gcloud_include

# did.txt
# https://theptrk.com/2018/07/11/did-txt-file/
: "${APS_DID_FILE:=~/Dropbox/Documents/notes/did.txt}"
alias did="vim +'normal Go' +'r!date' +'normal o' ${APS_DID_FILE}"
export PATH="/usr/local/opt/gettext/bin:$PATH"

peek() { tmux split-window -p 33 "$EDITOR" "$@" || exit; }
