# path
[ -z "$ANDROID_HOME" ] && export ANDROID_HOME=/usr/local/android-sdk/
[ -z "$APS_PATH" ] && export APS_PATH="$HOME/.dotfiles/bin:/usr/local/bin:/usr/local/sbin:/usr/local/heroku/bin:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"
[ -z "$DEFAULT_PATH" ] && export DEFAULT_PATH=$PATH
export PATH="$APS_PATH:$DEFAULT_PATH"
export MANPATH="/usr/local/man:/usr/local/mysql/man:/usr/local/git/man:$MANPATH"
