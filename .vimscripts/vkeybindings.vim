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

nnoremap <C-S-w> :w<CR>

map <S-t> :tabnew<CR>
map <leader>x :tabclose<CR>
map <C-t> :tabn<CR>
map <S-p> :tabp<CR>

nnoremap <silent> <C-d> :vertical resize +5<CR>
nnoremap <silent> <C-a> :vertical resize -5<CR>
nnoremap <silent> <C-x> :res +5<CR>
nnoremap <silent> <C-s> :res -5<CR>

nnoremap <leader>\ :vsplit<space>
nnoremap <leader>/ :split<space>

" Toggle line numbers
map <leader>n :set invnumber<CR>

" Unhighlight highlighted search results
map <silent> <leader><cr> :noh<CR>

" Opens a new tab with the current buffer's path
map <leader>te :tabedit <C-r>=expand(I0"%:p:h")<CR>/

""" Custom Commands """

" Copy/Paste to clipboard
vnoremap <C-c> :w !xclip -i -sel c<CR>

noremap <C-p> :r !xsel -o -b<CR>

" :W sudo saves the file
" (useful for handling the permission-denied error)
command! W execute 'w !sudo tee % > /dev/null' <bar> edit!
nmap <S-w> :W<CR>

nmap <F8> :TagbarToggle<CR>

nmap <leader>fm :Format<CR>

" Opens vim fugitive menu
nmap <leader>g :G<CR>
