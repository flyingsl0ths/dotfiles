" Vim Settings
set encoding=utf-8

set background=dark

set guifont="JetBrainsMonoMedium Nerd Font" 11

"Minimal number of lines to scroll when the cursor gets off the screen 
set scrolljump=5

"Maximum column in which to search for syntax items
set synmaxcol=130

" Height of the command bar
set cmdheight=2

" width (in spaces) that a <tab> is displayed as
set tabstop=4

" width (in spaces) used in each step of autoindent (aswell as << and >>)
set shiftwidth=4

set softtabstop=4

" Enable mouse support
set mouse=a

set modelines=4

" Entries showed in command history
set history=20

" A comma separated list of options for Insert mode completion
set completeopt="menu,menuone"

set backspace=indent,eol,start

" Make vim use the system clipboard:
set clipboard+=unnamedplus

" Disable audible bell
set noerrorbells visualbell t_vb=

set shell=/usr/bin/zsh

" Minimal number of columns to use for the line number
set numberwidth=4

set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/vendor/*,*/\.git/*

" Set to auto read when a file is changed from the outside
set autoread

augroup FocusGained,BufEnter * checktime
augroup END

" a <Tab> in front of a line inserts blanks according to 'shiftwidth'.
set smarttab

" expand tabs to spaces (use :retab to redo entire file)
set expandtab

" For regular expressions turn magic on
set magic

set modeline

set ttyfast

set gdefault

set number

set relativenumber

" Highlight search results
set hlsearch

set cursorline

set nostartofline

" Used when in search mode
set ignorecase
set smartcase

set title

set ruler

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
if has('nvim-0.5.0') || has('patch-8.1.1564')
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

" Directories to look for tag files in
set tags=tags;~

" No redraw while executing macros, registers and other commands that have not been typed.
set lazyredraw

set termguicolors

highlight Comment cterm=italic
"""""""""""""""""""""
