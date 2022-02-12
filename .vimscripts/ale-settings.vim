nmap <leader>af :ALEFix<CR>
nmap <silent>[a :ALENextWrap<CR>
nmap <silent>]a :ALEPreviousWrap<CR>
nnoremap <silent> <C-i>  :ALEInfo<CR>

let g:ale_disable_lsp = 1

let g:ale_fixers = { 
            \'*': 'trim_whitespace',
            \'sh' : 'shfmt',
            \'markdown': 'prettier',
            \'lua' : 'lua-format'
            \}

let g:ale_sign_error = '✗'
let g:ale_sign_warning = ''
