" set up pathogen, https://github.com/tpope/vim-pathogen
"
set nocompatible                                             " don't bother with vi compatibility
syntax enable                                                " enable syntax highlighting
let mapleader = ','

set autoindent
set autoread                                                 " reload files when changed on disk, i.e. via `git checkout`
set backspace=2                                              " Fix broken backspace in some setups
set backupcopy=yes                                           " see :help crontab
set cursorline                                               " highlight current line
set directory-=.                                             " don't store swapfiles in the current directory
set encoding=utf-8
set expandtab                                                " expand tabs to spaces
set ignorecase                                               " case-insensitive search
set incsearch                                                " search as you type
set laststatus=2                                             " always show statusline
set list                                                     " show trailing whitespace
set listchars=tab:▸\ ,trail:▫
set nowrap
set number                                                   " show line numbers
set pastetoggle=<leader>p
set ruler                                                    " show where you are
set scrolloff=3                                              " show context above/below cursorline
set shiftwidth=2                                             " normal mode indentation commands use 2 spaces
set showcmd
set showmatch                                                " jump to matching brackets on insertion
set smartcase                                                " case-sensitive search if any caps
set softtabstop=2                                            " insert mode tab and backspace use 2 spaces
set tabstop=8                                                " actual tabs occupy 8 characters
set timeoutlen=1000 ttimeoutlen=0                            " speed
set t_ti= t_te=                                              " Prevent Vim from clobbering the scrollback buffer. See http://www.shallowsky.com/linux/noaltscreen.html
set wildignore=log/**,node_modules/**,target/**,tmp/**,*.rbc
set wildmenu                                                 " show a navigable menu for tab completion
set wildmode=longest,list,full

" Enable basic mouse behavior such as resizing buffers.
set mouse=a
if exists('$TMUX')  " Support resizing in tmux
  set ttymouse=xterm2
endif

" plugin configuration
filetype on " without this vim emits a zero exit status, later, because of :ft off
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'kien/ctrlp.vim'
Plugin 'godlygeek/tabular'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-cucumber'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fugitive'
Plugin 'austintaylor/vim-indentobject'
Plugin 'pangloss/vim-javascript'
Plugin 'rmanalan/jshint.vim'
Plugin 'kchmck/vim-coffee-script'
Plugin 'vim-ruby/vim-ruby'
Plugin 'groenewege/vim-less'
Plugin 'tpope/vim-surround'
Plugin 'elixir-lang/vim-elixir'
Plugin 'mileszs/ack.vim'
Plugin 'mattn/gist-vim'
Plugin 'mattn/webapi-vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tpope/vim-vinegar'
Plugin 'davidoc/taskpaper.vim'
Plugin 'wting/rust.vim'
Plugin 'jnwhiteh/vim-golang'
Plugin 'bling/vim-airline'

call vundle#end()
filetype plugin indent on

" keyboard shortcuts
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <leader>l :Align
nmap <leader>a :Ack<space>
nmap <leader>b :CtrlPBuffer<CR>
nmap <leader>d :NERDTreeToggle<CR>
nmap <leader>f :NERDTreeFind<CR>
nmap <leader>t :CtrlP<CR>
nmap <leader>T :CtrlPClearCache<CR>:CtrlP<CR>
nmap <leader>] :TagbarToggle<CR>
nmap <leader><space> :call whitespace#strip_trailing()<CR>
nmap <leader>g :GitGutterToggle<CR>
nmap <leader>c <Plug>Kwbd
map <silent> <leader>V :source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>
map <leader>w :w!<cr>
inoremap jj <ESC>

" color setup
:set t_Co=256 " 256 colors
set background=dark
try
  colorscheme solarized
catch /^Vim\%((\a\+)\)\=:E185/
  color aps256
endtry

" tmux and iTerm2 cursor fun
" via @andyfowler https://gist.github.com/1195581
if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" plugin settings
let g:ctrlp_match_window = 'order:ttb,max:20'
let g:NERDSpaceDelims=1
let g:gitgutter_enabled = 0

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
  let g:ackprg = 'ag --nogroup --column'

  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

" fdoc is yaml
autocmd BufRead,BufNewFile *.fdoc set filetype=yaml
" md is markdown
autocmd BufRead,BufNewFile *.md set filetype=markdown
" automatically rebalance windows on vim resize
autocmd VimResized * :wincmd =
" ruby files
autocmd BufNewFile,BufRead Gemfile set filetype=ruby
autocmd BufNewFile,BufRead Podfile set filetype=ruby
autocmd BufNewFile,BufRead *.ru set filetype=ruby
" gradle
autocmd BufNewFile,BufRead *.gradle setf groovy

if filereadable(".vim.custom")
  so .vim.custom
endif

" Configure for a nicer prose writing environment
nnoremap <leader>P :call ProseToggle()<cr>
command! Prose :call ProseToggle()
let g:prose_is_toggled = 0
function! ProseToggle()
  if g:prose_is_toggled
    set nowrap
    nunmap j
    nunmap k
    let g:prose_is_toggled = 0
  else
    set wrap linebreak nolist
    noremap j gj
    noremap k gk
    let g:prose_is_toggled = 1
  endif
endfunction
