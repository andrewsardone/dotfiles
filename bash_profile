source ~/.bash/aliases
source ~/.bash/completions
source ~/.bash/config
source ~/.bash/paths

if [ -f ~/.localrc ]; then
  . ~/.localrc
fi

# if [ -f /etc/bashrc ]; then
#    . /etc/bashrc
# fi