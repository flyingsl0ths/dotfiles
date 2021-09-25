""""""""" Vim Settings
set encoding=utf-8

set background=dark

set guifont="JetBrainsMonoMedium Nerd Font" 11

" Height of the command bar
set cmdheight=2

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

set shell=/usr/bin/zsh

" Set to auto read when a file is changed from the outside
set autoread

augroup FocusGained,BufEnter * checktime
augroup END

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
set hlsearch

set cursorline

set nostartofline

" Used when in search mode
set ignorecase
set smartcase

set title

set ruler

set ignorecase

set incsearch

set nobackup

set nowritebackup

set noswapfile

set wildmenu

"Auto indent
set autoindent

"Smart indent
set smartindent

"Wrap lines
set wrap

filetype off                  " required

" Enables syntax highlighting
syntax enable

" Hides current mode in gutter
set noshowmode

set hidden

set updatetime=300

set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("nvim-0.5.0") || has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

let base16colorspace=256

" Default to static completion for SQL
let g:omni_sql_default_compl_type = 'syntax'

" Removes 'Error - SQLComplete: The debxt plugin must be loaded for dynamic SQL completion'
let g:omni_sql_no_default_maps = 1

set tags=tags;~
"""""""""""""""""""""

""""" Plugin Settings 
" nerdtree git
let g:NERDTreeGitStatusUseNerdFonts = 1
"""""""""""""""""""""
