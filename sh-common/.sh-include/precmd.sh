# Stores all history into log files
# via http://blog.andrewhays.net/2012/11/29/love-your-terminal.html
function precmd() {
  log_dir="$HOME/.logs"
  mkdir -p $log_dir
  if [ "$(id -u)" -ne 0 ]; then
    FULL_CMD_LOG="$log_dir/zsh-history-$(date -u "+%Y-%m-%d").log"
    case "$SHELL" in
      *bash*)
        most_recent_cmd="`\history 1`"
      ;;
      *zsh*)
        most_recent_cmd="`\history -1`"
      ;;
      *)
        most_recent_cmd=""
      ;;
    esac
    echo "$USER@`hostname`:`pwd` [$(date -u)] ${most_recent_cmd}" >> ${FULL_CMD_LOG}
  fi
}

