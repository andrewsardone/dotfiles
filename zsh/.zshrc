# path
[ -z "$ANDROID_HOME" ] && export ANDROID_HOME=/usr/local/android-sdk/
[ -z "$APS_PATH" ] && export APS_PATH="$HOME/bin:/usr/local/bin:/usr/local/sbin:/usr/local/heroku/bin:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"
[ -z "$DEFAULT_PATH" ] && export DEFAULT_PATH=$PATH
export PATH="$APS_PATH:$DEFAULT_PATH"
export MANPATH="/usr/local/man:/usr/local/mysql/man:/usr/local/git/man:$MANPATH"

fpath=( "$HOME/.zfunctions" $fpath )

# zshrc reloading
alias reload!='source ~/.zshrc'

# 10ms for key sequences
KEYTIMEOUT=1

# colors
export LSCOLORS="exfxcxdxbxegedabagacad"
export CLICOLOR=true

# sensible ls
alias ls="ls -hG"
alias l="ls -a"
alias ll="ls -la"

function aps_smart_ls {
  clear && pwd
  if [[ `ls -a $* | wc -l` -lt 40 ]]; then
    ll $*
  else
    l $*
  fi
}
alias sl=aps_smart_ls

# navigation
function aps_pushd {
  pushd $1 && aps_smart_ls
}
alias f=aps_pushd

function aps_popd {
  popd && aps_smart_ls
}
alias d=aps_popd

alias fh=f ~
alias cdh=cd ~ && pwd

# lolXcode
alias ded='rm -rf ~/Library/Developer/Xcode/DerivedData/'

# editor / readline setup
export EDITOR=vim
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
autoload -U colors && colors
autoload -U promptinit && promptinit
prompt pure

# pure prompt config
PURE_GIT_PULL=0

# Fish shell like syntax highlighting for Zsh
source $HOME/.zfunctions/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# history
setopt APPEND_HISTORY # adds history
setopt INC_APPEND_HISTORY SHARE_HISTORY  # adds history incrementally and share it across sessions
setopt HIST_IGNORE_ALL_DUPS  # don't record dupes in history
setopt HIST_REDUCE_BLANKS
export HISTSIZE=100000 SAVEHIST=100000 HISTFILE=~/.zhistory

# Stores all history into log files
# via http://blog.andrewhays.net/love-your-terminal
function precmd() {
  log_dir="$HOME/.logs"
  mkdir -p $log_dir
  if [ "$(id -u)" -ne 0 ]; then
    FULL_CMD_LOG="$log_dir/zsh-history-$(date -u "+%Y-%m-%d").log"
    echo "$USER@`hostname`:`pwd` [$(date -u)] `\history -1`" >> ${FULL_CMD_LOG}
  fi
}

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
if [ ! -z "$GRC_BASHRC" ]; then
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
export GOPATH=$HOME/code/gopath
export PATH=$PATH:$GOPATH/bin

nutshell_zshrc="$HOME/.nutshell.zshrc"
[ -f $nutshell_zshrc ] && source $nutshell_zshrc

# asdf – Extendable programming language version manager
# https://github.com/asdf-vm/asdf
asdf_sh=$HOME/.asdf/asdf.sh
[ -f $asdf_sh ] && source $asdf_sh
asdf_completions_sh=$HOME/.asdf/completions/asdf.bash
[ -f $asdf_completions_sh ] && source $asdf_completions_sh

# awscli completion
awscli_functions=/usr/local/share/zsh/site-functions/_aws
[ -f $awscli_functions ] && source $awscli_functions

# Mac App Store cli config
# https://github.com/mas-cli/mas
alias mas='reattach-to-user-namespace mas'

# fzf
# https://github.com/junegunn/fzf
export FZF_DEFAULT_OPTS='--height 40% --reverse --border --multi'
export FZF_DEFAULT_COMMAND='ag -g ""'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# fshow - git commit browser (enter for show, ctrl-d for diff, ` toggles sort)
# via https://junegunn.kr/2015/03/browsing-git-commits-with-fzf/
fshow() {
  local out shas sha q k
  while out=$(
      git log --graph --color=always \
          --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
      fzf --ansi --multi --no-sort --reverse --query="$q" \
          --print-query --expect=ctrl-d --toggle-sort=\`); do
    q=$(head -1 <<< "$out")
    k=$(head -2 <<< "$out" | tail -1)
    shas=$(sed '1,2d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')
    [ -z "$shas" ] && continue
    if [ "$k" = ctrl-d ]; then
      git diff --color=always $shas | less -R
    else
      for sha in $shas; do
        git show --color=always $sha | less -R
      done
    fi
  done
}

export GPG_TTY=$(tty)

# pass
# https://www.passwordstore.org/

# alias `pass` to fix the following error:
#   sed: RE error: illegal byte sequence
# See https://stackoverflow.com/a/23584470 for more info
alias pass='LC_ALL=C pass'
