nmap <leader>af :ALEFix<CR>

let g:ale_disable_lsp = 1

let g:ale_fixers = { '*':
			\'trim_whitespace',
			\'sh' : 'shfmt',
			\'cmake' : 'cmakeformat',
			\'python': ['black', 'reorder-python-imports'],
			\ 'ruby': 'rubocop',
            \ 'sql': 'sqlfmt'}
