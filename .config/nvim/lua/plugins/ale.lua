local vim_globals = vim.g

-- Only run linters named in ale_linters settings.
vim_globals.ale_linters_explicit = 1

vim_globals.ale_disable_lsp = true

vim_globals.ale_sign_error = '✗'

vim_globals.ale_sign_warning = ''

vim_globals.ale_set_highlights = 0

vim_globals.ale_linters = {["zsh"] = "shellcheck"}

vim_globals.ale_fixers = {}
