# bashrc reloading
alias reload!='source ~/.bashrc'

# Source the shell scripts and such that I share between bash and zsh.
sh_common_dir=$HOME/.sh-include
if [ -d $sh_common_dir ]; then
  for include in $sh_common_dir/*; do
    source "$include"
  done
fi

# editor / readline setup
export EDITOR=vim
set -o vi
bind -m vi-insert '"\C-x\C-e": edit-and-execute-command'

# fzf https://github.com/junegunn/fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Set history length
HISTFILESIZE=1000000000
HISTSIZE=1000000

# Append to the history file, don't overwrite it
shopt -s histappend
# Check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS
shopt -s checkwinsize
# Autocorrect typos in path names when using `cd`
shopt -s cdspell
# Save all lines of a multiple-line command in the same history entry (allows
# easy re-editing of multi-line commands)
shopt -s cmdhist
# Do not autocomplete when accidentally pressing Tab on an empty line. (It
# takes forever and yields "Display all 15 gazillion possibilites?")
shopt -s no_empty_cmd_completion

# Locale
export LC_ALL=en_US.UTF-8
export LANG="en_US"

# Colors
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"
MAGENTA="$(tput setaf 5)"
CYAN="$(tput setaf 6)"
WHITE="$(tput setaf 7)"
GRAY="$(tput setaf 8)"
BOLD="$(tput bold)"
UNDERLINE="$(tput sgr 0 1)"
INVERT="$(tput sgr 1 0)"
NOCOLOR="$(tput sgr0)"

# Tell ls to be colourful
export CLICOLOR=1

# Tell grep to highlight matches
alias grep='grep --color=auto'
