# path
[ -z "$ANDROID_HOME" ] && export ANDROID_HOME=/usr/local/android-sdk/
[ -z "$APS_PATH" ] && export APS_PATH="$HOME/.dotfiles/bin:$HOME/.toolbox/bin:/usr/local/opt/openjdk@11/bin:/usr/local/bin:/usr/local/sbin:/usr/local/heroku/bin:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"
[ -z "$DEFAULT_PATH" ] && export DEFAULT_PATH=$PATH

export PATH="$APS_PATH:$DEFAULT_PATH"
export MANPATH="/usr/local/man:/usr/local/mysql/man:/usr/local/git/man:$MANPATH"

# put kitty on path
# https://sw.kovidgoyal.net/kitty/
kitty_include='/Applications/kitty.app/Contents/MacOS'
[ -d $kitty_include ] && export PATH="$kitty_include:$PATH"
