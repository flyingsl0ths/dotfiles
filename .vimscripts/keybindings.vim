let mapleader = ' '

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

" Remaps keys used to move buffers around
noremap  <S-Left>  <C-w>r
nnoremap <S-Right>  <C-w>r
nnoremap <S-Up>  <C-w>H
nnoremap <S-Down>  <C-w>J

nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>

nnoremap <leader>bb :buffers<cr>:b<space>

nnoremap <C-p> :w<CR>

map <S-t> :tabnew<space>
map <leader>x :tabclose<CR>
map <C-t> :tabn<CR>
map <S-p> :tabp<CR>

nnoremap <silent> <C-d> :vertical resize +5<CR>
nnoremap <silent> <C-a> :vertical resize -5<CR>
nnoremap <silent> <C-x> :res +5<CR>
nnoremap <silent> <C-s> :res -5<CR>

nnoremap <leader>[ :vsplit<space>
nnoremap <leader>] :split<space>

" Toggle line numbers
map <leader>n :set invnumber<CR>

" Unhighlight highlighted search results
map <silent> <leader><cr> :noh<CR>

map <silent><leader>s :setlocal spell!<CR>

" Copy/Paste to clipboard
vmap <leader>e "+y

map <leader>p "+p

" :W sudo saves the file
" (useful for handling the permission-denied error)
command! W execute 'w !sudo tee % > /dev/null' <bar> edit!
nmap <S-w> :W<CR>

nmap <C-g> :TagbarToggle<CR>

nmap <leader>fm :Format<CR>

" Opens vim fugitive menu
nmap <leader>g :G<CR>


"" Vimspector
fun GotoWindow(id)
	call win_gotoid(a:id)
	MaximizerToggle
endfun

nnoremap <silent><leader>m  :MaximizerToggle!<CR>
nnoremap <leader>dd :call  vimspector#Launch()<CR>
nnoremap <leader>dc :call  GotoWindow(g:vimspector_session_windows.code)<CR>
nnoremap <leader>dt :call  GotoWindow(g:vimspector_session_windows.tagpage)<CR>
nnoremap <leader>dv :call  GotoWindow(g:vimspector_session_windows.variables)<CR>
nnoremap <leader>dw :call  GotoWindow(g:vimspector_session_windows.watches)<CR>
nnoremap <leader>ds :call  GotoWindow(g:vimspector_session_windows.stack_trace)<CR>
nnoremap <leader>do :call  GotoWindow(g:vimspector_session_windows.output)<CR>
nnoremap <leader>de :call  vimspector#Reset()<CR>
nnoremap <leader>dtcb :call vimspector#CleanLineBreakPoint()<CR>

nmap <leader>dl <Plug>VimspectorStepInto
nmap <leader>dj <Plug>VimspectorStepOver
nmap <leader>dk <Plug>VimspectorStepOut
nmap <leader>d_ <Plug>VimspectorRestart
nnoremap <space>d<space> :call vimspector#Continue()<CR>

nmap <leader>drc <Plug>VimspectorRunToCursor
nmap <leader>dbp <Plug>VimspectorToggleBreakpoint
nmap <leader>dcbp <Plug>VimspectorToggleConditionalBreakpoint
nmap <leader>wv :VimspectorWatch<space>
