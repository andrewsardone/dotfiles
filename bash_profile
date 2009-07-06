LC_ALL=en_US.UTF-8

PATH=$PATH:$HOME/bin:/usr/local/bin
source ~/.git-completion.sh

if [ -f $HOME/.alias.custom ]; then
   . $HOME/.alias.custom
fi

if [ -f $HOME/.atyponrc ]; then
   . $HOME/.atyponrc
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
   . /etc/bashrc
fi

DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}
export DYLD_LIBRARY_PATH

export LC_ALL PATH IP_ADDRESS

export PATH=/opt/local/bin:$PATH

# User Installed Shared Developer libraries
export SHARED_DEV_LIB=/Users/Shared/Developer/lib

# JRuby
export JRUBY_HOME=$SHARED_DEV_LIB/jruby/current
export JRUBY_BIN=$JRUBY_HOME/bin
export PATH=$PATH:$JRUBY_BIN
export CLASSPATH=$CLASSPATH:`find -L $JRUBY_HOME/lib -name '*.jar'|tr '\n' ':'`.

# Shell Color
CLICOLOR="YES"; export CLICOLOR
# Magenta directories
#LSCOLORS="fxgxcxdxbxegedabagacad"
# Green and Gold
LSCOLORS="dxfxcxdxbxegedabagacad"
export LSCOLORS

export PS1='[\u:\W]$ '

# lesspipe-1.55
LESSOPEN="|/usr/local/bin/lesspipe.sh %s"; export LESSOPEN

# Terminal Window Title
export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}: ${PWD/#$HOME/~}\007"'

# export EDITOR="mate -w"

# Default options for the zip command
# zip ZIPOPTS archive.zip source
ZIPOPT="-rug"; export ZIPOPT

# Objective-J / Cappuccino
export OBJJ_HOME="/usr/local/share/objj"
export STEAM_BUILD="$HOME/build/objj"