set encoding=utf-8

set background=dark

set guifont="JetBrainsMonoMedium Nerd Font" 11

" Height of the command bar
set cmdheight=1

" width (in spaces) that a <tab> is displayed as
set tabstop=4

" width (in spaces) used in each step of autoindent (aswell as << and >>)
set shiftwidth=4

" Enable mouse support
set mouse=a

set modelines=4

" Entries showed in command history
set history=20

set completeopt-=preview

set backspace=indent,eol,start

" Disable audible bell
set noerrorbells visualbell t_vb=

set shell=/bin/zsh

" Set to auto read when a file is changed from the outside
set autoread
au FocusGained,BufEnter * checktime

set smarttab

" expand tabs to spaces (use :retab to redo entire file)
set expandtab

" For regular expressions turn magic on
set magic

set modeline

set ttyfast

set gdefault

set number

" Highlight search results
set hls

set cursorline

set nostartofline

" Used when in search mode
set ignorecase
set smartcase

set title

set ruler

set ic

set is

set nobackup

set nowb

set noswapfile

set wildmenu

"Auto indent
set ai

"Smart indent
set si

"Wrap lines
set wrap

set nocompatible              " be iMproved, required

filetype off                  " required

" Enables syntax highlighting
syntax enable

" Hides current mode in gutter
set noshowmode

let base16colorspace=256

" Default to static completion for SQL
let g:omni_sql_default_compl_type = 'syntax'

" Removes 'Error - SQLComplete: The debxt plugin must be loaded for dynamic SQL completion'
let g:omni_sql_no_default_maps = 1

" Key mappings
" :W sudo saves the file
" (useful for handling the permission-denied error)
command! W execute 'w !sudo tee % > /dev/null' <bar> edit!

nmap Q <Nop>

" Used to prevent use of arrow keys
" In normal mode
nnoremap <Left>  :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up>    :echoe "Use k"<CR>
nnoremap <Down>  :echoe "Use j"<CR>

" Remaps keys used to switch between panes to ctrl-movementkeys
nnoremap <C-J> <C-w><C-j>
nnoremap <C-K> <C-w><C-k>
nnoremap <C-L> <C-w><C-l>
nnoremap <C-H> <C-w><C-h>

nmap <F8> :TagbarToggle<CR>
nmap <leader>f :NERDTree<CR>

map <leader>tn :tabnew<CR>
map <leader>to :tabonly<CR>
map <leader>tc :tabclose<CR>
map <leader>tm :tabmove<CR>
map <leader>n :tabn<CR>
map <leader>p :tabp<CR>

" Unhighlight highlighted search results
map <silent> <leader><cr> :noh<cr>

" Opens a new tab with the current buffer's path
map <leader>te :tabedit <C-r>=expand("%:p:h")<cr>/

" Fast saving
nmap <leader>w :w!<cr>

" Copy/Paste to clipboard
vnoremap <C-c> :w !xclip -i -sel c<CR><CR>

noremap <C-p> :r !xsel -o -b<CR><CR>

"

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Vundle managed
Plugin 'VundleVim/Vundle.vim'
Plugin 'itchyny/vim-gitbranch'
Plugin 'dense-analysis/ale'
Plugin 'itchyny/lightline.vim'
Plugin 'maximbaz/lightline-ale'
Plugin 'preservim/tagbar'
Plugin 'preservim/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'ryanoasis/vim-devicons'
Plugin 'ycm-core/YouCompleteMe'
Plugin 'udalov/kotlin-vim'
Plugin 'doums/darcula'
Plugin 'chriskempson/base16-vim'
"

" nerdtree git
let g:NERDTreeGitStatusUseNerdFonts = 1
"

" ALE Settings
let g:ale_fixers = { '*':
			\'trim_whitespace',
			\'sh' : 'shfmt',
			\'haskell' : 'brittany',
			\'c' : ['clang-format','clangtidy'],
			\'cpp': ['clang-format','clangtidy'],
			\'cmake' : 'cmakeformat',
			\'java': 'clang-format',
			\'markdown': 'prettier',
			\'html': 'prettier',
			\'python': ['black', 'reorder-python-imports'],
			\ 'ruby': 'rubocop',
            \ 'sql': 'sqlfmt'}

let g:ale_fix_on_save=1
let g:ale_haskell_ghc_options = '-fno-code -v0 -dynamic'
"

" YCM Settings
augroup MyYCMCustom
  autocmd!
  autocmd FileType c,cpp,python,java,javascript,typescript let b:ycm_hover = {
    \ 'command': 'GetDoc',
    \ 'syntax': &filetype
    \ }
augroup END
"

" Lightline Settings
set laststatus=2
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified', 'gitbranch'] ],
      \   'right': [
      \              ['lineinfo'],
      \              ['percent'],
      \              [ 'fileformat', 'fileencoding', 'filetype'],
      \              [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ] ]
      \ },
      \ 'component_function': {
      \ 'gitbranch': 'gitbranch#name',
      \ 'linter_checking': 'lightline#ale#checking',
      \ 'linter_infos': 'lightline#ale#infos',
      \ 'linter_warnings': 'lightline#ale#warnings',
      \ 'linter_errors': 'lightline#ale#errors',
      \ 'linter_ok': 'lightline#ale#ok'
      \ },
      \ }
"

call vundle#end()            " required
filetype plugin indent on    " required

set termguicolors
colorscheme base16-nord

