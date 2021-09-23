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

let base16colorspace=256

" Default to static completion for SQL
let g:omni_sql_default_compl_type = 'syntax'

" Removes 'Error - SQLComplete: The debxt plugin must be loaded for dynamic SQL completion'
let g:omni_sql_no_default_maps = 1

" Key mappings
let mapleader = ' '

" :W sudo saves the file
" (useful for handling the permission-denied error)
command! W execute 'w !sudo tee % > /dev/null' <bar> edit!

" Used to prevent use of arrow keys
" In normal mode
nnoremap <Left>  :echo "Use h"<CR>
nnoremap <Right> :echo "Use l"<CR>
nnoremap <Up>    :echo "Use k"<CR>
nnoremap <Down>  :echo "Use j"<CR>

" Remaps keys used to switch between panes to ctrl-movementkeys
nnoremap <C-J> <C-w><C-j>
nnoremap <C-K> <C-w><C-k>
nnoremap <C-L> <C-w><C-l>
nnoremap <C-H> <C-w><C-h>

" Used to exit Deol terminal buffer
tnoremap <ESC>   <C-\><C-n>

" Remaps keys used to move buffers around
nnoremap <C-Left>  <C-w>r
nnoremap <C-Right> <C-w>r
nnoremap <C-Up>    <C-w>H
nnoremap <C-d>     <C-w>J
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>

nnoremap <leader>bb :buffers<cr>:b<space>

nmap <F8> :TagbarToggle<CR>
nmap <leader>fm :ALEFix<CR>
nmap <leader>f :NERDTreeToggle<CR>
nmap <leader>ft :NERDTreeFocus<CR>

" Fast saving
nmap <C-s> :w!<CR>

map <S-t> :tabnew<CR>
map <leader>x :tabclose<CR>
map <C-n> :tabn<CR>
map <S-p> :tabp<CR>

" Toggle line numbers
map <leader>n :set invnumber<CR>

" Unhighlight highlighted search results
map <silent> <leader><cr> :noh<CR>

" Opens a new tab with the current buffer's path
map <leader>te :tabedit <C-r>=expand("%:p:h")<CR>/

" Copy/Paste to clipboard
vnoremap <C-c> :w !xclip -i -sel c<CR>

noremap <C-p> :r !xsel -o -b<CR>
"

" set the runtime path to include Vundle and initialize
set runtimepath+=~/.vim/bundle/Vundle.vim
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
Plugin 'udalov/kotlin-vim'
Plugin 'doums/darcula'
Plugin 'chriskempson/base16-vim'
Plugin 'Shougo/deol.nvim'
Plugin 'chrisbra/Colorizer'
Plugin 'ctrlpvim/ctrlp.vim'
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
			\'yaml': 'prettier',
			\'html': 'prettier',
			\'python': ['black', 'reorder-python-imports'],
			\ 'ruby': 'rubocop',
            \ 'sql': 'sqlfmt'}
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

let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'i:instance:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type',
        \ 'i' : 'instance'
    \ },
    \ 'scope2kind' : {
        \ 'module'   : 'm',
        \ 'class'    : 'c',
        \ 'data'     : 'd',
        \ 'type'     : 't',
        \ 'instance' : 'i'
    \ }
\ }

let g:tagbar_type_kotlin = {
    \ 'ctagstype' : 'kotlin',
    \ 'ctagsbin' : 'ctags-universal',
    \ 'kinds'     : [
        \ 'c:classes:0:1',
        \ 'f:functions',
        \ 'g:enums',
        \ 'u:unions',
        \ 's:structs',
        \ 'm:members'
    \ ],
    \'sro': '.',
    \ 'kind2scope' : {
        \ 'c' : 'class',
        \ 'g' : 'enum',
        \ 's' : 'struct',
        \ 'u' : 'union'
    \},
    \ 'scope2kind' : {
        \ 'enum'      : 'g',
        \ 'class'     : 'c',
        \ 'struct'    : 's',
        \ 'union'     : 'u'
    \ }
\ }


let g:colorizer_auto_filetype='css,html,dosini,sh,conf,rasi'

" ctrlp Settings
" Key binding to run command
let g:ctrlp_map = '<C-f>'

" Used to specifiy root directory of all searches
" by using the cwd of any file in the list
let g:ctrlp_root_markers = ['pom.xml', 'package.yaml', 'gradlew', 'compile_commands.json', '.config']

" Open file already opened, in new buffer instead of switching to it
let g:ctrlp_switch_buffer = 'et'

" Exclude files in .gitignore
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

let g:ctrlp_custom_ignore = 'home/flyingsl0ths'
"

call vundle#end()            " required
filetype plugin indent on    " required

set termguicolors
colorscheme base16-nord
