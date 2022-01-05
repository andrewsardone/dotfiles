set fish_greeting ""

# environment
set -gx EDITOR nvim
set -gx PATH /opt/homebrew/sbin $PATH
set -gx PATH /opt/homebrew/bin $PATH
set -gx PATH /nix/var/nix/profiles/default/bin $PATH
set -gx PATH ~/.nix-profile/bin $PATH
set -gx PATH ~/.toolbox/bin $PATH
set -gx PATH ~/bin $PATH
set -gx PATH ~/.local/bin $PATH
set -gx PATH bin $PATH
set -gx TERM xterm-256color

# aliases
alias l "ls -a"
alias ll "ls -la"
alias vi vim
command -qv bat; and alias cat "bat --paging=never"
command -qv btm; and alias btm "btm -b --mem_as_value"
command -qv ncdu; and alias ncdu "ncdu --color dark"
command -qv lsd; and alias ls lsd
if command -qv nvim
  alias vim nvim
  alias vi nvim
  alias view "nvim -R"
end

# editing
set -g fish_key_bindings fish_vi_key_bindings

# prompt
if command -qv starship
  starship init fish | source
end

# theme
theme_gruvbox "dark" "hard"
set -U fish_color_command c397d8
set -U fish_color_autosuggestion 969896

# smart ls
function aps_smart_ls
  clear
  pwd
  if test (ls -a $argv | wc -l) -lt 40
    ll $argv
  else
    l $argv
  end
end
alias sl aps_smart_ls

# navigation
function aps_pushd
  pushd $argv[1] && aps_smart_ls
end
alias f aps_pushd

function aps_popd
  popd && aps_smart_ls
end
alias d aps_popd
