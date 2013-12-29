# path
[ -z "$APS_PATH" ] && export APS_PATH="$HOME/bin:/usr/local/bin:/usr/local/sbin:/usr/local/heroku/bin"
[ -z "$DEFAULT_PATH" ] && export DEFAULT_PATH=$PATH
export PATH="$APS_PATH:$DEFAULT_PATH"
export MANPATH="/usr/local/man:/usr/local/mysql/man:/usr/local/git/man:$MANPATH"

fpath=( "$HOME/.zfunctions" $fpath )

# zshrc reloading
alias reload!='source ~/.zshrc'

# rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

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

function block_cursor() { print -n -- "\EPtmux;\E\E]50;CursorShape=0\C-G\E\\" }
function line_cursor() { print -n -- "\EPtmux;\E\E]50;CursorShape=1\C-G\E\\" }

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

# history
setopt APPEND_HISTORY # adds history
setopt INC_APPEND_HISTORY SHARE_HISTORY  # adds history incrementally and share it across sessions
setopt HIST_IGNORE_ALL_DUPS  # don't record dupes in history
setopt HIST_REDUCE_BLANKS
export HISTSIZE=100000 SAVEHIST=100000 HISTFILE=~/.zhistory

# Stores all history into log files
# via http://blog.andrewhays.net/love-your-terminal
function precmd() {
  if [ "$(id -u)" -ne 0 ]; then
    FULL_CMD_LOG="$HOME/.logs/zsh-history-$(date -u "+%Y-%m-%d").log"
    echo "$USER@`hostname`:`pwd` [$(date -u)] `\history -1`" >> ${FULL_CMD_LOG}
  fi
}

# pager
export LESS="-RIM"

# rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# A Poor Man's Notational Velocity
# via http://onethingwell.org/post/457674798/a-poor-mans-notational-velocity
function n {
  vi "~/Dropbox/Documents/Notes/$*.txt"
}

function ns {
  if [ "$#" == "0" ]; then
    ls -c ~/Dropbox/Documents/Notes
  else
    ls -c ~/Dropbox/Documents/Notes | grep $1
  fi
}

# nutshell
eval "$(/Users/andrew/source/nutshell/nub/bin/nub init -)"

# autocomplete
autoload -U compinit && compinit

# Task management

# list TODO/FIX lines from the current project
alias todos="ack --nogroup '(TODO|FIX(ME)?):'"

# create a Taskpaper todo file in the current folder
# via http://brettterpstra.com/2013/03/31/a-few-more-of-my-favorite-shell-aliases/
alias tp='touch todo.taskpaper && open -a "Taskpaper" todo.taskpaper'

# grc

GRC_BASHRC="`brew --prefix`/etc/grc.bashrc"
if [ -f $GRC_BASHRC ]; then
  source $GRC_BASHRC
fi

# via http://thomashunter.name/blog/removing-duplicate-entries-from-finders-open-with-menu/
alias reset-finder-open-with-menu="/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister -kill -r -domain local -domain system -domain user; killall Finder"

# hub, which makes git better with github http://defunkt.io/hub/
alias git=hub

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
