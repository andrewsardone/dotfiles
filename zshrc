# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="prose"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=~/bin:/usr/local/homebrew/bin:/usr/local/bin:$PATH
alias emacsx="open -a Emacs.app"
alias em="emacsclient -nw"
alias mate="open -a TextMate.app"
alias reload_textmate_bundles="osascript -e 'tell app \"TextMate\" to reload bundles'"
alias dock2d="defaults write com.apple.dock no-glass -boolean YES; killall Dock"
alias dock3d="defaults write com.apple.dock no-glass -boolean NO; killall Dock"
