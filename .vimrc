" Variables
set encoding=utf-8

set tabstop=4     " (ts) width (in spaces) that a <tab> is displayed as
set expandtab     " (et) expand tabs to spaces (use :retab to redo entire file)
set shiftwidth=4  " (sw) width (in spaces) used in each step of autoindent (aswell as << and >>)

" Enable mouse support
set mouse=a

set modelines=4
set history=500
set completeopt-=preview
set backspace=indent,eol,start

" Disable audible bell
set noerrorbells visualbell t_vb=

set shell=/bin/zsh

set modeline
set ttyfast
set gdefault
set number
set hls
set cursorline
set nostartofline
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
set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines
set nocompatible              " be iMproved, required
filetype off                  " required
syntax enable
set noshowmode
"

" Key mappings
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


" Tagbar mappings
nmap <F8> :TagbarToggle<CR>
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
Plugin 'chriskempson/base16-vim'
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
			\ 'ruby': 'rubocop'}

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
      \ 'colorscheme': 'Tomorrow_Night_Eighties',
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
      \   'gitbranch': 'gitbranch#name',
      \  'linter_checking': 'lightline#ale#checking',
      \  'linter_infos': 'lightline#ale#infos',
      \  'linter_warnings': 'lightline#ale#warnings',
      \  'linter_errors': 'lightline#ale#errors',
      \  'linter_ok': 'lightline#ale#ok'
      \ },
      \ }
"

call vundle#end()            " required
filetype plugin indent on    " required

set termguicolors
colorscheme base16-gruvbox-dark-soft
