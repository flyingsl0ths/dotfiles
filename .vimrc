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
nmap <leader>fm :Format<CR>
nmap <leader>t :NERDTreeToggle<CR>
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
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'neoclide/coc.nvim'
"

" nerdtree git
let g:NERDTreeGitStatusUseNerdFonts = 1
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
      \              ['cocstatus'] ]
      \ },
      \ 'component_function': {
      \ 'gitbranch': 'gitbranch#name',
      \ 'cocstatus' : 'coc#status'
      \ },
      \ }

" Use autocmd to force lightline update.
autocmd User CocStatusChange,CocDiagnosticChange call lightline#update()
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

" ctrlp Settings
" Key binding to run command
let g:ctrlp_map = '<S-f>'

" Used to specifiy root directory of all searches
" by using the cwd of any file in the list
let g:ctrlp_root_markers = ['pom.xml', 'package.yaml', 'gradlew', 'compile_commands.json', '.config']

" Open file already opened, in new buffer instead of switching to it
let g:ctrlp_switch_buffer = 'et'

" Exclude files in .gitignore
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

let g:ctrlp_custom_ignore = 'home/flyingsl0ths'
"

" COC settings
" Use tab for trigger completion with characters ahead and navigate.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-@> coc#refresh()


" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . ' ' . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType haskell,python,cpp,c,java,kotlin,typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>af  <Plug>(coc-fix-current)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-r> <Plug>(coc-range-select)
xmap <silent> <C-r> <Plug>(coc-range-select)

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>
"

call vundle#end()            " required
filetype plugin indent on    " required

set termguicolors
colorscheme base16-nord
