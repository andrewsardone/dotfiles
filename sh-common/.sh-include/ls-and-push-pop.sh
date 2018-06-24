# sensible ls
alias ls="ls -hG --color"
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

alias fh="f ~"
alias cdh="cd ~ && pwd"
