# Setup fzf
# ---------
if [[ ! "$PATH" == */opt/homebrew/opt/fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/opt/homebrew/opt/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/opt/homebrew/opt/fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/opt/homebrew/opt/fzf/shell/key-bindings.zsh"

# # Enhanced fzf functions for glob-based file searching
# # Ctrl+T for file search with glob patterns
# export FZF_CTRL_T_COMMAND='fd --type f --hidden --follow --exclude .git --exclude node_modules'
# export FZF_CTRL_T_OPTS='--preview "head -50 {}" --preview-window=right:60%:wrap'
#
# # Alt+C for directory search
# export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git --exclude node_modules'
# export FZF_ALT_C_OPTS='--preview "ls -la {}" --preview-window=right:60%'
#
# # Custom glob search function - search files by pattern
# fzf-glob() {
#   local pattern="${1:-**/*}"
#   local selected=$(fd --type f --hidden --follow --exclude .git --exclude node_modules | grep -E "$pattern" | fzf --preview "head -50 {}" --preview-window=right:60%:wrap)
#   if [[ -n $selected ]]; then
#     BUFFER="$EDITOR $selected"
#     zle accept-line
#   fi
#   zle reset-prompt
# }
# zle -N fzf-glob
# bindkey '^G' fzf-glob  # Ctrl+G to search with glob patterns
#
# # Search and edit files containing text
# fzf-grep() {
#   local query="$1"
#   if [[ -z $query ]]; then
#     echo "Usage: fzf-grep <search_term>"
#     return 1
#   fi
#   local selected=$(rg --files-with-matches --hidden --follow --glob '!.git/*' --glob '!node_modules/*' "$query" | fzf --preview "rg --color=always --line-number --context=3 '$query' {}" --preview-window=right:60%:wrap)
#   if [[ -n $selected ]]; then
#     $EDITOR "$selected"
#   fi
# }
#
# # Interactive file finder with preview
# ff() {
#   local pattern="${1:-}"
#   local search_path="${2:-.}"
#
#   if [[ -n $pattern ]]; then
#     fd --type f --hidden --follow --exclude .git --exclude node_modules "$pattern" "$search_path" | fzf --preview "head -50 {}" --preview-window=right:60%:wrap
#   else
#     fd --type f --hidden --follow --exclude .git --exclude node_modules . "$search_path" | fzf --preview "head -50 {}" --preview-window=right:60%:wrap
#   fi
# }
#
# # Open file with default editor after fzf selection
# fe() {
#   local file=$(ff "$@")
#   if [[ -n $file ]]; then
#     $EDITOR "$file"
#   fi
# }
#
# # Find and cd to directory
# fcd() {
#   local dir=$(fd --type d --hidden --follow --exclude .git --exclude node_modules | fzf --preview "ls -la {}" --preview-window=right:60%)
#   if [[ -n $dir ]]; then
#     aps_pushd "$dir"
#   fi
# }
