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
      \              [ 'cocstatus', 
      \                'linter_checking', 'linter_errors',
      \                'linter_warnings', 'linter_infos',
      \                'linter_ok' ] ]
      \ },
      \ 'component_function': {
      \ 'gitbranch': 'gitbranch#name',
      \ 'linter_checking': 'lightline#ale#checking',
      \ 'linter_infos': 'lightline#ale#infos',
      \ 'linter_warnings': 'lightline#ale#warnings',
      \ 'linter_errors': 'lightline#ale#errors',
      \ 'linter_ok': 'lightline#ale#ok',
      \ 'cocstatus' : 'coc#status',
      \ },
      \ }

" Use autocmd to force lightline update.
autocmd User CocStatusChange,CocDiagnosticChange call lightline#update()
