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

nmap <leader>t :NERDTreeToggle<CR>

nmap <leader>ft :NERDTreeFocus<CR>

" Opens vim fugitive menu
nmap <leader>g :G<CR>
