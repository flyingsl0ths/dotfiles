nmap <leader>af :ALEFix<CR>
nmap <silent>[g :ALENextWrap<CR>
nmap <silent>]g :ALEPreviousWrap<CR>

let g:ale_disable_lsp = 1

let g:ale_fixers = { '*':
			\'trim_whitespace',
            \'sh' : 'shfmt'}

" Allows jump between linter errors
nmap <silent> [a :ALEFirst <CR>
nmap <silent> ]a :ALELast <CR>
