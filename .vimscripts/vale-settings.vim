nmap <leader>af :ALEFix<CR>
nmap <silent>[g :ALENextWrap<CR>
nmap <silent>]g :ALEPreviousWrap<CR>

let g:ale_disable_lsp = 1

let g:ale_fixers = { '*':
			\'trim_whitespace',
            \'sh' : 'shfmt'}

let g:ale_sign_error = '✗'
let g:ale_sign_warning = ''
