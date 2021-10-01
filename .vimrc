so ~/.vimscripts/vpreferences.vim

so ~/.vimscripts/vkeybindings.vim

" set the runtime path to include Vundle and initialize
set runtimepath+=~/.vim/bundle/Vundle.vim
call vundle#begin()

so ~/.vimscripts/vale-settings.vim

" Vundle managed
Plugin 'VundleVim/Vundle.vim'
Plugin 'itchyny/vim-gitbranch'
Plugin 'itchyny/lightline.vim'
Plugin 'maximbaz/lightline-ale'
Plugin 'dense-analysis/ale'
Plugin 'preservim/tagbar'
Plugin 'preservim/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'ryanoasis/vim-devicons'
Plugin 'Shougo/deol.nvim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'udalov/kotlin-vim'
Plugin 'neoclide/coc.nvim'
Plugin 'xolox/vim-misc'
Plugin 'chriskempson/base16-vim'
Plugin 'tpope/vim-fugitive'

so ~/.vimscripts/vlightline-settings.vim

so ~/.vimscripts/vctrlp-settings.vim

so ~/.vimscripts/vcoc-settings.vim

so ~/.vimscripts/vtagbar-settings.vim

call vundle#end()            " required
filetype plugin indent on    " required

set termguicolors
colorscheme base16-nord
