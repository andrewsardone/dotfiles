# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.dotfiles/oh-my-zsh  # shouldn't be a hard coded path...

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="andrewsardone"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=~/bin:/usr/local/Cellar/python/2.7/bin:/usr/local/bin:/usr/local/sbin:$PATH
alias ls="ls -hG"
alias mate="open -a TextMate.app"
alias reload_textmate_bundles="osascript -e 'tell app \"TextMate\" to reload bundles'"
alias dock2d="defaults write com.apple.dock no-glass -boolean YES; killall Dock"
alias dock3d="defaults write com.apple.dock no-glass -boolean NO; killall Dock"
alias ql="qlmanage -p"
alias t='python /usr/local/bin/t.py --task-dir ~/Dropbox/tasks --list task'

export EDITOR="emacsclient -nw"

# Emacs setup
alias em="emacsclient -nw"  # Open a new Emacs frame on the current terminal
function emacs() {
  emacsclient -n $@ > /dev/null 2>&1 || open -a Emacs.app $@
}

# Xcode convenience function
function xcode() {
  if (( $# == 0 )); then
    open *.xcodeproj
    return
  fi
  open -a Xcode.app $1
}

if [[ $TERM = "eterm-color" ]]; then
   export TERM="xterm"
fi

#autojump
#Copyright Joel Schaerer 2008, 2009
#This file is part of autojump

#autojump is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#autojump is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with autojump.  If not, see <http://www.gnu.org/licenses/>.

function autojump_preexec() {
    { (autojump -a "$(pwd -P)"&)>/dev/null 2>>|${HOME}/.autojump_errors ; } 2>/dev/null
}

typeset -ga preexec_functions
preexec_functions+=autojump_preexec

alias jumpstat="autojump --stat"

function j { local new_path="$(autojump $@)";if [ -n "$new_path" ]; then echo -e "\\033[31m${new_path}\\033[0m"; cd "$new_path";fi }

# rvm
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm
