alias l="ls -la"
alias ls="ls -hG"

alias mate="open -a TextMate.app"

alias reload_textmate_bundles="osascript -e 'tell app \"TextMate\" to reload bundles'"

alias dock2d="defaults write com.apple.dock no-glass -boolean YES; killall Dock"
alias dock3d="defaults write com.apple.dock no-glass -boolean NO; killall Dock"

# Quicklook a file
alias ql="qlmanage -p"

# For an mds process using up a lot of CPU, erasing and 
# rebuilding the indexes for all volumes can fix the problem
alias reset_mds='sudo mdutil -aE'

alias reload!='. ~/.zshrc'

# Open a new Emacs frame on the current terminal
alias em="emacsclient"
