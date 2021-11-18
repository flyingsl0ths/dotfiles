so ~/.vimscripts/vpreferences.vim

so ~/.vimscripts/vkeybindings.vim

" set the runtime path to include Vundle and initialize
set runtimepath+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Vundle managed
" Plugin 'doums/darcula'
Plugin 'VundleVim/Vundle.vim'
Plugin 'itchyny/vim-gitbranch'
Plugin 'itchyny/lightline.vim'
Plugin 'maximbaz/lightline-ale'
Plugin 'dense-analysis/ale'
Plugin 'preservim/tagbar'
Plugin 'preservim/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'ryanoasis/vim-devicons'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'udalov/kotlin-vim'
Plugin 'neoclide/coc.nvim'
Plugin 'xolox/vim-misc'
Plugin 'chriskempson/base16-vim'
Plugin 'tpope/vim-fugitive'
Plugin 'mhinz/vim-startify'
Plugin 'puremourning/vimspector'

so ~/.vimscripts/vale-settings.vim

so ~/.vimscripts/vlightline-settings.vim

so ~/.vimscripts/vctrlp-settings.vim

so ~/.vimscripts/vcoc-settings.vim

so ~/.vimscripts/vtagbar-settings.vim

so ~/.vimscripts/vnerdtree-settings.vim

so ~/.vimscripts/vspector-settings.vim

call vundle#end()            " required
filetype plugin indent on    " required

colorscheme base16-nord

autocmd VimEnter *
            \   if !argc()
            \ |   Startify
            \ |   NERDTree
            \ |   wincmd w
            \ | endif
