so ~/.vimscripts/preferences.vim

so ~/.vimscripts/keybindings.vim

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
Plugin 'arcticicestudio/nord-vim'
Plugin 'tpope/vim-fugitive'
Plugin 'mhinz/vim-startify'
Plugin 'puremourning/vimspector'
Plugin 'szw/vim-maximizer'

so ~/.vimscripts/ale-settings.vim

so ~/.vimscripts/lightline-settings.vim

so ~/.vimscripts/ctrlp-settings.vim

so ~/.vimscripts/coc-settings.vim

so ~/.vimscripts/tagbar-settings.vim

so ~/.vimscripts/nerdtree-settings.vim

so ~/.vimscripts/vimspector-settings.vim

call vundle#end()            " required
filetype plugin indent on    " required

colorscheme nord

autocmd VimEnter *
            \   if !argc()
            \ |   Startify
            \ |   NERDTree
            \ |   wincmd w
            \ | endif
