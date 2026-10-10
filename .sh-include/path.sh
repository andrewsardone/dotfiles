# path
[ -z "$ANDROID_HOME" ] && export ANDROID_HOME=/usr/local/android-sdk/
[ -z "$APS_PATH" ] && export APS_PATH="$HOME/.dotfiles/bin:$HOME/.local/bin:/usr/local/opt/openjdk@11/bin:/usr/local/bin:/usr/local/sbin:/usr/local/heroku/bin:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"
[ -z "$DEFAULT_PATH" ] && export DEFAULT_PATH=$PATH

export PATH="$APS_PATH:$DEFAULT_PATH"
export MANPATH="/usr/local/man:/usr/local/mysql/man:/usr/local/git/man:$MANPATH"

# put ghostty on path
# https://ghostty.org/
ghostty_include='/Applications/Ghostty.app/Contents/MacOS'
[ -d $ghostty_include ] && export PATH="$ghostty_include:$PATH"
